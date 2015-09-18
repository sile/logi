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
              Layout = ?MODULE,
              ?assert(logi_layout:is_layout(Layout))
      end},
     {"Creates a layout: module + extra-data",
      fun () ->
              Layout = {?MODULE, hoge},
              ?assert(logi_layout:is_layout(Layout))
      end},
     {"A module which does not implement `logi_layout' behaviour is not regarded as a layout",
      fun () ->
              Layout = lists,
              ?assertNot(logi_layout:is_layout(Layout))
      end}
    ].

format_test_() ->
    Context = logi_context:new(test_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}),
    [
     {"Formats data",
      fun () ->
              Layout = {?MODULE, "extra"},
              ?assertEqual("hello world: extra", logi_layout:format(Context, "hello ~s", [world], Layout))
      end},
     {"The default value of `extra_data()' is `undefined'",
      fun () ->
              Layout = ?MODULE,
              ?assertEqual("hello world: undefined", logi_layout:format(Context, "hello ~s", [world], Layout))
      end}
    ].
