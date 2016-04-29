%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_sink_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a sink specification",
      fun () ->
              Sink = logi_sink:new(#{id => dummy, start => {dummy, start_link, []}}),
              ?assert(logi_sink:is_sink(Sink))
      end}
    ].

get_test_() ->
    [
     {"Gets the information from a sink",
      fun () ->
              Sink = logi_sink:new(#{id => dummy, start => {dummy, start_link, []}}),
              ?assertEqual(dummy, logi_sink:get_id(Sink)),
              ?assertMatch(#{id := dummy, start := {dummy, start_link, []}}, logi_sink:get_spec(Sink))
      end}
    ].
