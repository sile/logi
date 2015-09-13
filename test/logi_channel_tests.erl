%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_channel_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(NULL_SINK, logi_builtin_sink_null).

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
    NullSink = logi_sink:new(null, ?NULL_SINK),
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
                 ?assertEqual(error, logi_channel:find_sink(Channel, null)),
                 ?assertEqual([], logi_channel:select_sink(Channel, info, hoge, fuga))
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
                 AnotherSink = logi_sink:new(null, ?NULL_SINK, info),
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

select_test_() ->
    Channel = test_channel,
    Install =
        fun (Id, Condition) ->
                Sink = logi_sink:new(Id, ?NULL_SINK, Condition, Id),
                {ok, _} = logi_channel:install_sink(Channel, Sink),
                ok
        end,
    SetCond =
        fun (Id, Condition) ->
                {ok, _} = logi_channel:set_condition(Channel, Id, Condition),
                ok
        end,
    Select =
        fun (Severity, Application, Module) ->
                lists:sort(logi_channel:select_sink(Channel, Severity, Application, Module))
        end,
    {setup,
     fun () -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {foreach,
       fun () -> ok = logi_channel:create(Channel) end,
       fun (_) -> ok = logi_channel:delete(Channel) end,
       [
        {"simple",
         fun () ->
                 ok = Install(aaa, info),
                 ok = Install(bbb, debug),

                 ?assertEqual([{?NULL_SINK, bbb}],                        Select(debug,   stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, bbb}],                        Select(verbose, stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(info,    stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(alert,   stdlib, lists))
         end},
        {"range",
         fun () ->
                 ok = Install(aaa, {debug, info}),
                 ok = Install(bbb, {verbose, critical}),

                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(debug,   stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(verbose, stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(info,    stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, bbb}],                        Select(notice,  stdlib, lists)),
                 ?assertEqual([],                                             Select(alert,   stdlib, lists))
         end},
        {"list",
         fun () ->
                 ok = Install(aaa, [debug, info, notice]),
                 ok = Install(bbb, [verbose, notice, critical]),
                 ok = Install(ccc, [info]),

                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(debug,   stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, bbb}],                        Select(verbose, stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, ccc}], Select(info,    stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(notice,  stdlib, lists)),
                 ?assertEqual([],                                             Select(alert,   stdlib, lists))
         end},
        {"severity + application",
         fun () ->
                 ok = Install(aaa, #{severity => debug,          application => stdlib}),
                 ok = Install(bbb, #{severity => [info, notice], application => [stdlib, kernel]}),
                 ok = Install(ccc, #{severity => verbose,        application => kernel}),

                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(debug,   stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(verbose, stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(info,    stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(notice,  stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(alert,   stdlib, lists))
         end},
        {"severity + application + module",
         fun () ->
                 ok = Install(aaa, #{severity => debug,          application => kernel, module => [lists, dict]}),
                 ok = Install(bbb, #{severity => [info, notice], application => stdlib, module => net_kernel}),
                 ok = Install(ccc, #{severity => verbose,        application => kernel, module => net_kernel}),

                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(debug,   stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(verbose, stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(info,    stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(notice,  stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(alert,   stdlib, lists))
         end},
        {"change condition",
         fun () ->
                 ok = Install(aaa, debug),
                 ok = Install(bbb, info),
                 ok = Install(ccc, alert),
                 ok = Install(ddd, emergency),

                 random:seed(os:timestamp()),
                 Applications = applications(),
                 Modules = modules(),
                 lists:foreach(
                   fun (_) ->
                           ok = SetCond(aaa, random_condition(Applications, Modules)),
                           ok = SetCond(bbb, random_condition(Applications, Modules)),
                           ok = SetCond(ccc, random_condition(Applications, Modules)),
                           ok = SetCond(ddd, random_condition(Applications, Modules))
                   end,
                   lists:seq(1, 100)),

                 ok = SetCond(aaa, #{severity => debug,          module => [lists, dict]}),
                 ok = SetCond(bbb, #{severity => [info, notice], application => stdlib, module => net_kernel}),
                 ok = SetCond(ccc, #{severity => verbose,        application => kernel, module => net_kernel}),
                 ok = SetCond(ddd, emergency),

                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(debug,   stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(verbose, stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(info,    stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}, {?NULL_SINK, bbb}], Select(notice,  stdlib, lists)),
                 ?assertEqual([{?NULL_SINK, aaa}],                        Select(alert,   stdlib, lists))
         end}
       ]}
     ]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec random_condition([atom()], [module()]) -> logi_sink:condition().
random_condition(Applications, Modules) ->
    Levels = logi:log_levels(),
    case random:uniform(3) of
        1 -> subshuffle(Levels);
        2 -> #{severity => subshuffle(Levels), application => subshuffle(Applications)};
        3 -> #{severity => subshuffle(Levels), application => subshuffle(Applications), module => subshuffle(Modules)}
    end.

-spec subshuffle(list()) -> list().
subshuffle(List) ->
    lists:sublist([X || {_, X} <- lists:sort([{random:uniform(), X} || X <- List])],
                  random:uniform(max(20, length(List))) -1).

-spec applications() -> [atom()].
applications() ->
    [A || {A, _, _} <- application:which_applications()].

-spec modules() -> [module()].
modules() ->
    [M || {M, _} <- code:all_loaded(), application:get_application(M) =/= undefined].
