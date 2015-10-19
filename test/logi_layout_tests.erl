%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_layout_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(logi_layout).

-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% `logi_layout` Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
format(_Context, Format, Data, Extra) ->
    lists:flatten(io_lib:format(Format ++ ": ~s", Data ++ [Extra])).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
create_test_() ->
    [
     {"Creates a layout: module only",
      fun () ->
              Layout = logi_layout:new(?MODULE),
              ?assert(logi_layout:is_layout(Layout))
      end},
     {"Creates a layout: module + extra-data",
      fun () ->
              Layout = logi_layout:new(?MODULE, hoge),
              ?assert(logi_layout:is_layout(Layout))
      end},
     {"A module which does not implement `logi_layout' behaviour is not regarded as a layout",
      fun () ->
              ?assertNot(logi_layout:is_layout(lists)),
              ?assertError(badarg, logi_layout:new(lists))
      end}
    ].

get_test_() ->
    Layout0 = logi_layout:new(?MODULE),
    Layout1 = logi_layout:new(?MODULE, "extra"),
    [
     ?_assertEqual(?MODULE, logi_layout:get_module(Layout0)),
     ?_assertEqual(?MODULE, logi_layout:get_module(Layout1)),
     ?_assertEqual(undefined, logi_layout:get_extra_data(Layout0)),
     ?_assertEqual("extra",   logi_layout:get_extra_data(Layout1))
    ].

format_test_() ->
    Context = logi_context:new(test_log, info),
    [
     {"Formats data",
      fun () ->
              Layout = logi_layout:new(?MODULE, "extra"),
              ?assertEqual("hello world: extra", logi_layout:format(Context, "hello ~s", [world], Layout))
      end},
     {"The default value of `extra_data()' is `undefined'",
      fun () ->
              Layout = logi_layout:new(?MODULE),
              ?assertEqual("hello world: undefined", logi_layout:format(Context, "hello ~s", [world], Layout))
      end}
    ].
