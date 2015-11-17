%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_sink_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(TEST_SINK, logi_builtin_sink_fun).
-define(LAYOUT, logi_builtin_layout_pass_through:new()).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a sink specification",
      fun () ->
              Sink = logi_sink:new(?TEST_SINK, ?LAYOUT, extra_data),
              ?assert(logi_sink:is_sink(Sink))
      end}
    ].

get_test_() ->
    [
     {"Gets the information from a sink",
      fun () ->
              S = logi_sink:new(?TEST_SINK, ?LAYOUT, extra),
              ?assertEqual(?TEST_SINK, logi_sink:get_module(S)),
              ?assertEqual(?LAYOUT,    logi_sink:get_layout(S)),
              ?assertEqual(extra,      logi_sink:get_extra_data(S))
      end}
    ].
