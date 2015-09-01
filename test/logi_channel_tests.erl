%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_channel_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
channel_test_() ->
    Channel = test_channel,
    {setup,
     fun () -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {"The predefined channel",
       fun () ->
               ?assertEqual([logi_channel:default_channel()], logi_channel:which_channels())
       end},
      {"Creates a channel",
       fun () ->
               ?assertEqual(ok,        logi_channel:create(Channel)),
               ?assertEqual([Channel], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ?assertEqual(ok,        logi_channel:delete(Channel)),
               ?assertEqual([],        lists:delete(logi_channel:default_channel(), logi_channel:which_channels()))
       end},
      {"CREATE: If the channel already exists, nothing happens",
       fun () ->
               ok = logi_channel:create(Channel),
               ?assertEqual([Channel], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ?assertEqual(ok,        logi_channel:create(Channel)),
               ?assertEqual([Channel], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ok = logi_channel:delete(Channel)
       end},
      {"DELETE: If the channel does exists, it will be silently ignored",
       fun () ->
               ?assertEqual([], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ?assertEqual(ok, logi_channel:delete(Channel)),
               ?assertEqual([], lists:delete(logi_channel:default_channel(), logi_channel:which_channels()))
       end},
      {"ERROR(badarg): non atom id",
       fun () ->
               ?assertError(badarg, logi_channel:create("hoge")),
               ?assertError(badarg, logi_channel:delete(<<"fuga">>))
       end},
      {"ERROR(badarg): id conflict",
       fun () ->
               true = register(existing_process_name, spawn(timer, sleep, [infinity])),
               ?assertError(badarg, logi_channel:create(existing_process_name)),

               _ = ets:new(existing_table_name, [named_table]),
               ?assertError(badarg, logi_channel:create(existing_table_name))
       end}
     ]}.

sink_test_() ->
    Channel = test_channel,
    NullSink = logi_sink:new(null, logi_sink_null),
    {setup,
     fun () -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {"Initially no sinks are installed",
       fun () ->
               ?assertEqual([], logi_channel:which_sinks(logi_channel:default_channel()))
       end},
      {foreach,
       fun () -> ok = logi_channel:create(Channel) end,
       fun (_) -> ok = logi_channel:delete(Channel) end,
       [
        {"Installs a sink",
         fun () ->
                 ?assertEqual({ok, undefined}, logi_channel:install_sink(Channel, NullSink)),
                 ?assertEqual([null], logi_channel:which_sinks(Channel))
         end},
        {"Finds a sink",
         fun () ->
                 ?assertEqual(error, logi_channel:find_sink(Channel, null)),
                 {ok, undefined} = logi_channel:install_sink(Channel, NullSink),
                 ?assertEqual({ok, NullSink}, logi_channel:find_sink(Channel, null))
         end},
        {"Uninstalls a sink",
         fun () ->
                 {ok, undefined} = logi_channel:install_sink(Channel, NullSink),
                 ?assertEqual({ok, NullSink}, logi_channel:uninstall_sink(Channel, null)),
                 ?assertEqual(error, logi_channel:uninstall_sink(Channel, null)),
                 ?assertEqual(error, logi_channel:find_sink(Channel, null))
         end},
        {"INSTALL: `if_exists` option",
         fun () ->
                 {ok, undefined} = logi_channel:install_sink(Channel, NullSink),

                 %% if_exists == error
                 ?assertEqual({error, {already_installed, NullSink}},
                              logi_channel:install_sink(Channel, NullSink, [{if_exists, error}])),

                 %% if_exists == ignore
                 ?assertEqual({ok, NullSink},
                              logi_channel:install_sink(Channel, NullSink, [{if_exists, ignore}])),

                 %% if_exists == supersede
                 AnotherSink = logi_sink:new(null, logi_sink_null, info),
                 ?assertEqual({ok, NullSink},
                              logi_channel:install_sink(Channel, AnotherSink, [{if_exists, supersede}])),
                 ?assertNotEqual(NullSink, AnotherSink),
                 ?assertEqual({ok, AnotherSink}, logi_channel:find_sink(Channel, null))
         end},
        {"INSTALL: `lifetime` option",
         fun () ->
                 %% lifetime == 50
                 {ok, undefined} = logi_channel:install_sink(Channel, NullSink, [{lifetime, 50}]),
                 ?assertMatch({ok, _}, logi_channel:find_sink(Channel, null)),
                 timer:sleep(100),
                 ?assertEqual(error, logi_channel:find_sink(Channel, null)),

                 %% lifetime == pid()
                 {Pid, Ref} = spawn_monitor(timer, sleep, [infinity]),
                 {ok, undefined} = logi_channel:install_sink(Channel, NullSink, [{lifetime, Pid}]),
                 ?assertMatch({ok, _}, logi_channel:find_sink(Channel, null)),
                 exit(Pid, kill),
                 receive {'DOWN', Ref, _, _, _} -> ok end,
                 ?assertEqual(error, logi_channel:find_sink(Channel, null))
         end},
        {"set_condition/3",
         fun () ->
                 ?assertEqual(error, logi_channel:set_condition(Channel, null, info)),

                 {ok, undefined} = logi_channel:install_sink(Channel, NullSink),
                 ?assertEqual({ok, debug}, logi_channel:set_condition(Channel, null, info)),

                 {ok, Sink} = logi_channel:find_sink(Channel, null),
                 ?assertEqual(info, logi_sink:get_condition(Sink))
         end}
       ]}
     ]}.
