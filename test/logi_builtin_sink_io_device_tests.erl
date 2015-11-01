%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_sink_io_device_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new sink instance",
      fun () ->
              Sink = logi_builtin_sink_io_device:new(),
              ?assert(logi_sink:is_sink(Sink))
      end}
    ].
