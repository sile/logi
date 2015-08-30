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
      {"Trys stopping an unexisting logger",
       fun () ->
               ?assertEqual([logi:default_logger()], logi:which_loggers()),
               ?assertEqual(ok, logi:stop_logger(hoge_logger))
       end}
     ]}.
