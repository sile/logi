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
    NullSink = logi_sink:new(?NULL_SINK),
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
                 ?assertEqual({ok, undefined}, logi_channel:install_sink(debug, NullSink, [{channel, Channel}])),
                 ?assertEqual([?NULL_SINK], logi_channel:which_sinks(Channel))
         end},
        {"Finds a sink",
         fun () ->
                 ?assertEqual(error, logi_channel:find_sink(Channel, ?NULL_SINK)),
                 {ok, undefined} = logi_channel:install_sink(debug, NullSink, [{channel, Channel}]),
                 ?assertEqual({ok, {debug, logi_sink:default_layout(NullSink), NullSink}},
                              logi_channel:find_sink(Channel, ?NULL_SINK))
         end},
        {"Uninstalls a sink",
         fun () ->
                 {ok, undefined} = logi_channel:install_sink(debug, NullSink, [{channel, Channel}]),
                 ?assertEqual({ok, {debug, logi_sink:default_layout(NullSink), NullSink}},
                              logi_channel:uninstall_sink(Channel, ?NULL_SINK)),
                 ?assertEqual(error, logi_channel:uninstall_sink(Channel, ?NULL_SINK)),
                 ?assertEqual(error, logi_channel:find_sink(Channel, ?NULL_SINK)),
                 ?assertEqual([], logi_channel:select_sink(Channel, info, hoge, fuga))
         end},
        {"INSTALL: `if_exists` option",
         fun () ->
                 {ok, undefined} = logi_channel:install_sink(debug, NullSink, [{channel, Channel}]),

                 %% if_exists == error
                 ?assertEqual({error, {already_installed, {debug, logi_sink:default_layout(NullSink), NullSink}}},
                              logi_channel:install_sink(debug, NullSink, [{channel, Channel}, {if_exists, error}])),

                 %% if_exists == ignore
                 ?assertEqual({ok, {debug, logi_sink:default_layout(NullSink), NullSink}},
                              logi_channel:install_sink(info, NullSink, [{channel, Channel}, {if_exists, ignore}])),

                 %% if_exists == supersede
                 AnotherSink = logi_sink:new(?NULL_SINK, extra),
                 ?assertEqual({ok, {debug, logi_sink:default_layout(NullSink), NullSink}},
                              logi_channel:install_sink(info, AnotherSink, [{channel, Channel}, {if_exists, supersede}])),
                 ?assertNotEqual(NullSink, AnotherSink),
                 ?assertEqual({ok, {info, logi_sink:default_layout(NullSink), AnotherSink}},
                              logi_channel:find_sink(Channel, ?NULL_SINK)),

                 %% invalid value
                 ?assertError(badarg, logi_channel:install_sink(debug, NullSink, [{channel, Channel}, {if_exists, undefined}]))
         end},
        {"INSTALL: `lifetime` option",
         fun () ->
                 %% lifetime == 50
                 {ok, undefined} = logi_channel:install_sink(debug, NullSink, [{channel, Channel}, {lifetime, 50}]),
                 ?assertMatch({ok, _}, logi_channel:find_sink(Channel, ?NULL_SINK)),
                 timer:sleep(100),
                 ?assertEqual(error, logi_channel:find_sink(Channel, ?NULL_SINK)),

                 %% lifetime == pid()
                 {Pid, Ref} = spawn_monitor(timer, sleep, [infinity]),
                 {ok, undefined} = logi_channel:install_sink(debug, NullSink, [{channel, Channel}, {lifetime, Pid}]),
                 ?assertMatch({ok, _}, logi_channel:find_sink(Channel, ?NULL_SINK)),
                 exit(Pid, kill),
                 receive {'DOWN', Ref, _, _, _} -> ok end,
                 ?assertEqual(error, logi_channel:find_sink(Channel, ?NULL_SINK)),

                 %% invalid value
                 ?assertError(badarg, logi_channel:install_sink(debug, NullSink, [{channel, Channel}, {lifetime, -1}])),
                 ?assertError(badarg, logi_channel:install_sink(debug, NullSink, [{channel, Channel}, {lifetime, undefined}]))
         end},
        {"set_condition/3",
         fun () ->
                 ?assertEqual(error, logi_channel:set_condition(Channel, ?NULL_SINK, info)),

                 {ok, undefined} = logi_channel:install_sink(debug, NullSink, [{channel, Channel}]),
                 ?assertEqual({ok, debug}, logi_channel:set_condition(Channel, ?NULL_SINK, info)),

                 ?assertMatch({ok, {info, _, NullSink}}, logi_channel:find_sink(Channel, ?NULL_SINK))
         end}
       ]}
     ]}.

select_test_() ->
    Channel = test_channel,
    Install =
        fun (Id, Condition) ->
                Sink = logi_sink:new(?NULL_SINK, Id),
                {ok, _} = logi_channel:install_sink(Condition, Sink, [{channel, Channel}, {id, Id}]),
                ok
        end,
    SetCond =
        fun (Id, Condition) ->
                {ok, _} = logi_channel:set_condition(Channel, Id, Condition),
                ok
        end,
    Select =
        fun (Severity, Application, Module) ->
                lists:sort([Id || {_, Id, _} <- logi_channel:select_sink(Channel, Severity, Application, Module)])
        end,
    {setup,
     fun () -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {foreach,
       fun () -> ok = logi_channel:create(Channel) end,
       fun (_) -> ok = logi_channel:delete(Channel) end,
       [
        {"simple (level)",
         fun () ->
                 ok = Install(aaa, info),
                 ok = Install(bbb, debug),

                 ?assertEqual([bbb],      Select(debug,   stdlib, lists)),
                 ?assertEqual([bbb],      Select(verbose, stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(info,    stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(alert,   stdlib, lists))
         end},
        {"range",
         fun () ->
                 ok = Install(aaa, {debug, info}),
                 ok = Install(bbb, {verbose, critical}),

                 ?assertEqual([aaa],      Select(debug,   stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(verbose, stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(info,    stdlib, lists)),
                 ?assertEqual([bbb],      Select(notice,  stdlib, lists)),
                 ?assertEqual([],         Select(alert,   stdlib, lists))
         end},
        {"list",
         fun () ->
                 ok = Install(aaa, [debug, info, notice]),
                 ok = Install(bbb, [verbose, notice, critical]),
                 ok = Install(ccc, [info]),

                 ?assertEqual([aaa],      Select(debug,   stdlib, lists)),
                 ?assertEqual([bbb],      Select(verbose, stdlib, lists)),
                 ?assertEqual([aaa, ccc], Select(info,    stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(notice,  stdlib, lists)),
                 ?assertEqual([],         Select(alert,   stdlib, lists))
         end},
        {"severity + application",
         fun () ->
                 ok = Install(aaa, #{severity => debug,          application => stdlib}),
                 ok = Install(bbb, #{severity => [info, notice], application => [stdlib, kernel]}),
                 ok = Install(ccc, #{severity => verbose,        application => kernel}),

                 ?assertEqual([aaa],      Select(debug,   stdlib, lists)),
                 ?assertEqual([aaa],      Select(verbose, stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(info,    stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(notice,  stdlib, lists)),
                 ?assertEqual([aaa],      Select(alert,   stdlib, lists))
         end},
        {"severity + application + module",
         fun () ->
                 ok = Install(aaa, #{severity => debug,          application => kernel, module => [lists, dict]}),
                 ok = Install(bbb, #{severity => [info, notice], application => stdlib, module => net_kernel}),
                 ok = Install(ccc, #{severity => verbose,        application => kernel, module => net_kernel}),

                 ?assertEqual([aaa],      Select(debug,   stdlib, lists)),
                 ?assertEqual([aaa],      Select(verbose, stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(info,    stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(notice,  stdlib, lists)),
                 ?assertEqual([aaa],      Select(alert,   stdlib, lists))
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

                 ?assertEqual([aaa],      Select(debug,   stdlib, lists)),
                 ?assertEqual([aaa],      Select(verbose, stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(info,    stdlib, lists)),
                 ?assertEqual([aaa, bbb], Select(notice,  stdlib, lists)),
                 ?assertEqual([aaa],      Select(alert,   stdlib, lists))
         end}
       ]}
     ]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec random_condition([atom()], [module()]) -> logi_sink:condition().
random_condition(Applications, Modules) ->
    Severities = logi:severities(),
    case random:uniform(3) of
        1 -> subshuffle(Severities);
        2 -> #{severity => subshuffle(Severities), application => subshuffle(Applications)};
        3 -> #{severity => subshuffle(Severities), application => subshuffle(Applications), module => subshuffle(Modules)}
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
