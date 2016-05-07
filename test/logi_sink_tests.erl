%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_sink_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a sink",
      fun () ->
              Sink = logi_sink:new(#{id => dummy, start => {dummy, start_link, []}}),
              ?assert(logi_sink:is_sink(Sink))
      end}
    ].

from_writer_test_() ->
    [
     {"Creates a sink from a writer",
      fun () ->
              Writer = logi_sink_writer:new(logi_builtin_sink_fun, fun (_, _, _) -> [] end),
              Sink = logi_sink:from_writer(foo, Writer),
              ?assert(logi_sink:is_sink(Sink)),
              ?assertEqual(foo, logi_sink:get_id(Sink))
      end}
    ].

get_test_() ->
    [
     {"Gets the information from a sink",
      fun () ->
              %% map format
              Sink0 = logi_sink:new(#{id => dummy, start => {dummy, start_link, []}}, #{intensity => 2, period => 3}),
              ?assertEqual(dummy, logi_sink:get_id(Sink0)),
              ?assertMatch(#{id := dummy, start := {dummy, start_link, []}}, logi_sink:get_spec(Sink0)),
              ?assertMatch(#{intensity := 2, period := 3}, logi_sink:get_sup_flags(Sink0)),

              %% tuple format
              Sink1 = logi_sink:new({dummy, {dummy, start_link, []}, permanent, 10, worker, [dummy]}, {one_for_one, 2, 3}),
              ?assertEqual(dummy, logi_sink:get_id(Sink1)),
              ?assertMatch(#{id := dummy, start := {dummy, start_link, []}}, logi_sink:get_spec(Sink1)),
              ?assertMatch(#{intensity := 2, period := 3}, logi_sink:get_sup_flags(Sink1))
      end}
    ].
