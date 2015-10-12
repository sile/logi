%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_filter_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(logi_filter).

-export([filter/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% `logi_filter` Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
filter(Context, _State) ->
    maps:get(filter, logi_context:get_metadata(Context), true).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
create_test_() ->
    [
     {"Creates a filter: module only",
      fun () ->
              Filter = logi_filter:new(?MODULE),
              ?assert(logi_filter:is_filter(Filter))
      end},
     {"Creates a filter: module + state",
      fun () ->
              Filter = logi_filter:new(?MODULE, hoge),
              ?assert(logi_filter:is_filter(Filter))
      end},
     {"A module which does not implement `logi_filter' behaviour is not regarded as a filter",
      fun () ->
              ?assertNot(logi_filter:is_filter(lists)),
              ?assertError(badarg, logi_filter:new(lists))
      end}
    ].

get_test_() ->
    Filter0 = logi_filter:new(?MODULE),
    Filter1 = logi_filter:new(?MODULE, "state"),
    [
     ?_assertEqual(?MODULE, logi_filter:get_module(Filter0)),
     ?_assertEqual(?MODULE, logi_filter:get_module(Filter1)),
     ?_assertEqual(undefined, logi_filter:get_state(Filter0)),
     ?_assertEqual("state",   logi_filter:get_state(Filter1))
    ].

apply_test_() ->
    Context0 = logi_context:new(test_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{filter => true}),
    Context1 = logi_context:new(test_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{filter => false}),
    [
     {"Applies a filter",
      fun () ->
              Filter = logi_filter:new(?MODULE),
              ?assertEqual(true,  logi_filter:apply(Context0, Filter)),
              ?assertEqual(false, logi_filter:apply(Context1, Filter))
      end}
    ].
