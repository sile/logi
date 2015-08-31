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

%%----------------------------------------------------------
%% Logger
%%----------------------------------------------------------
logger_test_() ->
    {setup,
     fun () -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {"The default logger always exists",
       fun () ->
               ?assertEqual([logi:default_logger()], logi:which_loggers())
       end},
      {"Starts and stops a logger",
       fun () ->
               %% Starts
               ?assertMatch({ok, _}, logi:start_logger(hoge_logger)),
               ?assertEqual(lists:sort([logi:default_logger(), hoge_logger]), lists:sort(logi:which_loggers())),

               %% Stops
               ?assertEqual(ok, logi:stop_logger(hoge_logger)),
               ?assertEqual([logi:default_logger()], logi:which_loggers())
       end},
      {"Every logger must have a unique ID",
       fun () ->
               {ok, _} = logi:start_logger(hoge_logger),
               ?assertMatch({error, {already_started, _}}, logi:start_logger(hoge_logger)), % Duplicated ID
               ok = logi:stop_logger(hoge_logger)
       end},
      {"`logi:ensure_logger_started/1` allows duplicated logger ID",
       fun () ->
               {ok, Pid} = logi:ensure_logger_started(hoge_logger),
               ?assertEqual({ok, Pid}, logi:ensure_logger_started(hoge_logger)),
               ok = logi:stop_logger(hoge_logger)
       end},
      {"Tries stopping an unexisting logger",
       fun () ->
               ?assertEqual([logi:default_logger()], logi:which_loggers()),
               ?assertEqual(ok, logi:stop_logger(hoge_logger))
       end}
     ]}.

%%----------------------------------------------------------
%% Appender
%%----------------------------------------------------------
appender_test_() ->
    Logger = hoge_logger,
    NullAppender = logi_appender:make(null, logi_appender_null),
    {setup,
     fun () -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {"Initially no appenders are registered",
       fun () ->
               ?assertEqual([], logi:which_appenders(logi:default_logger()))
       end},
      {foreach,
       fun () -> {ok, _} = logi:start_logger(Logger) end,
       fun (_) -> ok = logi:stop_logger(Logger) end,
       [
        {"Registers an appender",
         fun () ->
                 ?assertEqual({ok, undefined}, logi:register_appender(Logger, NullAppender)),
                 ?assertEqual([null], logi:which_appenders(Logger))
         end},
        {"Finds an appender",
         fun () ->
                 ?assertEqual(error, logi:find_appender(Logger, null)),
                 {ok, undefined} = logi:register_appender(Logger, NullAppender),
                 ?assertEqual({ok, NullAppender}, logi:find_appender(Logger, null))
         end},
        {"Deregisters an appender",
         fun () ->
                 ?assertEqual(error, logi:deregister_appender(Logger, null)),
                 {ok, undefined} = logi:register_appender(Logger, NullAppender),
                 ?assertEqual({ok, NullAppender}, logi:deregister_appender(Logger, null)),
                 ?assertEqual(error, logi:find_appender(Logger, null))
         end},
        {"logi:register_appender/3: `if_exists` option",
         fun () ->
                 {ok, undefined} = logi:register_appender(Logger, NullAppender),

                 %% if_exists == error
                 ?assertEqual({error, {already_registered, NullAppender}},
                              logi:register_appender(Logger, NullAppender, #{if_exists => error})),

                 %% if_exists == ignore
                 ?assertEqual({ok, NullAppender},
                              logi:register_appender(Logger, NullAppender, #{if_exists => ignore})),

                 %% if_exists == supersede
                 AnotherAppender = logi_appender:make(null, logi_appender_null, info),
                 ?assertEqual({ok, NullAppender},
                              logi:register_appender(Logger, AnotherAppender, #{if_exists => supersede})),
                 ?assertNotEqual(NullAppender, AnotherAppender),
                 ?assertEqual({ok, AnotherAppender}, logi:find_appender(Logger, null))
         end},
        {"logi:register_appender/3: `lifetime` option",
         fun () ->
                 %% lifetime == 50
                 {ok, undefined} = logi:register_appender(Logger, NullAppender, #{lifetime => 50}),
                 ?assertMatch({ok, _}, logi:find_appender(Logger, null)),
                 timer:sleep(100),
                 ?assertEqual(error, logi:find_appender(Logger, null)),

                 %% lifetime == pid()
                 {Pid, Ref} = spawn_monitor(timer, sleep, [infinity]),
                 {ok, undefined} = logi:register_appender(Logger, NullAppender, #{lifetime => Pid}),
                 ?assertMatch({ok, _}, logi:find_appender(Logger, null)),
                 exit(Pid, kill),
                 receive {'DOWN', Ref, _, _, _} -> ok end,
                 ?assertEqual(error, logi:find_appender(Logger, null))
         end},
        {"logi:set_condition/3",
         fun () ->
                 ?assertEqual(error, logi:set_condition(Logger, null, info)),

                 {ok, undefined} = logi:register_appender(Logger, NullAppender),
                 ?assertEqual({ok, debug}, logi:set_condition(Logger, null, info)),

                 {ok, Appender} = logi:find_appender(Logger, null),
                 ?assertEqual(info, logi_appender:get_condition(Appender))
         end}
       ]}
     ]}.
