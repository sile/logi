%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
%%----------------------------------------------------------
%% Constants
%%----------------------------------------------------------
log_levels_test_() ->
    {"Available log leves",
     ?_assertEqual([debug, verbose, info, notice, warning, error, critical, alert, emergency], logi:log_levels())}.

default_logger_test_() ->
    {"The default logger",
     ?_assertEqual(logi_default_logger, logi:default_logger())}.

%% %%----------------------------------------------------------
%% %% Channel
%% %%----------------------------------------------------------
%% channel_test_() ->
%%     {setup,
%%      fun () -> ok = application:start(logi) end,
%%      fun (_) -> ok = application:stop(logi) end,
%%      [
%%       {"The default channel always exists",
%%        fun () ->
%%                ?assertEqual([logi:default_logger()], logi:which_channels())
%%        end},
%%       {"Starts and stops a channel",
%%        fun () ->
%%                %% Starts
%%                ?assertMatch({ok, _}, logi:start_channel(hoge_channel)),
%%                ?assertEqual(lists:sort([logi:default_logger(), hoge_channel]), lists:sort(logi:which_channels())),

%%                %% Stops
%%                ?assertEqual(ok, logi:stop_channel(hoge_channel)),
%%                ?assertEqual([logi:default_logger()], logi:which_channels())
%%        end},
%%       {"Every channel must have a unique ID",
%%        fun () ->
%%                {ok, _} = logi:start_channel(hoge_channel),
%%                ?assertMatch({error, {already_started, _}}, logi:start_channel(hoge_channel)), % Duplicated ID
%%                ok = logi:stop_channel(hoge_channel)
%%        end},
%%       {"`logi:ensure_channel_started/1` allows duplicated channel ID",
%%        fun () ->
%%                {ok, Pid} = logi:ensure_channel_started(hoge_channel),
%%                ?assertEqual({ok, Pid}, logi:ensure_channel_started(hoge_channel)),
%%                ok = logi:stop_channel(hoge_channel)
%%        end},
%%       {"Tries stopping an unexisting channel",
%%        fun () ->
%%                ?assertEqual([logi:default_logger()], logi:which_channels()),
%%                ?assertEqual(ok, logi:stop_channel(hoge_channel))
%%        end}
%%      ]}.

%% %%----------------------------------------------------------
%% %% Appender
%% %%----------------------------------------------------------
%% appender_test_() ->
%%     Channel = hoge_channel,
%%     NullAppender = logi_appender:make(null, logi_appender_null),
%%     {setup,
%%      fun () -> ok = application:start(logi) end,
%%      fun (_) -> ok = application:stop(logi) end,
%%      [
%%       {"Initially no appenders are registered",
%%        fun () ->
%%                ?assertEqual([], logi:which_appenders(logi:default_logger()))
%%        end},
%%       {foreach,
%%        fun () -> {ok, _} = logi:start_channel(Channel) end,
%%        fun (_) -> ok = logi:stop_channel(Channel) end,
%%        [
%%         {"Registers an appender",
%%          fun () ->
%%                  ?assertEqual({ok, undefined}, logi:register_appender(Channel, NullAppender)),
%%                  ?assertEqual([null], logi:which_appenders(Channel))
%%          end},
%%         {"Finds an appender",
%%          fun () ->
%%                  ?assertEqual(error, logi:find_appender(Channel, null)),
%%                  {ok, undefined} = logi:register_appender(Channel, NullAppender),
%%                  ?assertEqual({ok, NullAppender}, logi:find_appender(Channel, null))
%%          end},
%%         {"Deregisters an appender",
%%          fun () ->
%%                  ?assertEqual(error, logi:deregister_appender(Channel, null)),
%%                  {ok, undefined} = logi:register_appender(Channel, NullAppender),
%%                  ?assertEqual({ok, NullAppender}, logi:deregister_appender(Channel, null)),
%%                  ?assertEqual(error, logi:find_appender(Channel, null))
%%          end},
%%         {"logi:register_appender/3: `if_exists` option",
%%          fun () ->
%%                  {ok, undefined} = logi:register_appender(Channel, NullAppender),

%%                  %% if_exists == error
%%                  ?assertEqual({error, {already_registered, NullAppender}},
%%                               logi:register_appender(Channel, NullAppender, #{if_exists => error})),

%%                  %% if_exists == ignore
%%                  ?assertEqual({ok, NullAppender},
%%                               logi:register_appender(Channel, NullAppender, #{if_exists => ignore})),

%%                  %% if_exists == supersede
%%                  AnotherAppender = logi_appender:make(null, logi_appender_null, info),
%%                  ?assertEqual({ok, NullAppender},
%%                               logi:register_appender(Channel, AnotherAppender, #{if_exists => supersede})),
%%                  ?assertNotEqual(NullAppender, AnotherAppender),
%%                  ?assertEqual({ok, AnotherAppender}, logi:find_appender(Channel, null))
%%          end},
%%         {"logi:register_appender/3: `lifetime` option",
%%          fun () ->
%%                  %% lifetime == 50
%%                  {ok, undefined} = logi:register_appender(Channel, NullAppender, #{lifetime => 50}),
%%                  ?assertMatch({ok, _}, logi:find_appender(Channel, null)),
%%                  timer:sleep(100),
%%                  ?assertEqual(error, logi:find_appender(Channel, null)),

%%                  %% lifetime == pid()
%%                  {Pid, Ref} = spawn_monitor(timer, sleep, [infinity]),
%%                  {ok, undefined} = logi:register_appender(Channel, NullAppender, #{lifetime => Pid}),
%%                  ?assertMatch({ok, _}, logi:find_appender(Channel, null)),
%%                  exit(Pid, kill),
%%                  receive {'DOWN', Ref, _, _, _} -> ok end,
%%                  ?assertEqual(error, logi:find_appender(Channel, null))
%%          end},
%%         {"logi:set_condition/3",
%%          fun () ->
%%                  ?assertEqual(error, logi:set_condition(Channel, null, info)),

%%                  {ok, undefined} = logi:register_appender(Channel, NullAppender),
%%                  ?assertEqual({ok, debug}, logi:set_condition(Channel, null, info)),

%%                  {ok, Appender} = logi:find_appender(Channel, null),
%%                  ?assertEqual(info, logi_appender:get_condition(Appender))
%%          end}
%%        ]}
%%      ]}.

%% %%----------------------------------------------------------
%% %% Logger
%% %%----------------------------------------------------------
%% logger_instance_test_() ->
%%     Chanel = hoge_channel,
%%     [
%%      {"Makes a logger instance",
%%       fun () ->
%%               Logger = logi:new(#{channel_id => Chanel}),
%%               ?assertEqual(#{channel_id => Chanel}, logi:to_map(Logger))
%%       end},
%%      {"Makes a logger instance with options",
%%       fun () ->
%%               todo
%%       end}
%%     ].
