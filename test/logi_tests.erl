%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
log_levels_test_() ->
    {"Available log leves",
     ?_assertEqual([debug, verbose, info, notice, warning, error, critical, alert, emergency], logi:log_levels())}.

default_logger_test_() ->
    {"The default logger",
     ?_assertEqual(logi_default_log, logi:default_logger())}.

logger_test_() ->
    Chanel = hoge_channel,
    [
     {"Makes a logger instance",
      fun () ->
              Logger = logi:new(#{channel_id => Chanel}),
              ?assertEqual(#{channel_id => Chanel}, logi:to_map(Logger))
      end},
     {"Makes a logger instance with options",
      fun () ->
              todo
      end}
    ].
