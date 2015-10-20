%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_layout_fun_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a `logi_builtin_layout_fun' layout",
      fun () ->
              Layout = logi_builtin_layout_fun:new(fun (_, _, _) -> "hoge" end),
              ?assert(logi_layout:is_layout(Layout))
      end},
     {"[ERORR] not a `format_fun/0' is passed",
      fun () ->
              ?assertError(badarg, logi_builtin_layout_fun:new(hoge)), % not a function
              ?assertError(badarg, logi_builtin_layout_fun:new(fun lists:map/2))  % not a `format_fun/0' function
      end}
    ].

format_test_() ->
    Context = logi_context:new(test_log, info),
    [
     {"Formats a log message",
      fun () ->
              Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format(Format, Data) end),
              Format = "Hello ~s",
              Data = ["World"],
              ?assertEqual(io_lib:format(Format, Data), logi_layout:format(Context, Format, Data, Layout))
      end}
    ].
