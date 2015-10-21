%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_layout_simple_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a `logi_builtin_layout_simple' layout",
      fun () ->
              Layout = logi_builtin_layout_simple:new(),
              ?assert(logi_layout:is_layout(Layout))
      end}
    ].

format_test_() ->
    [
     {"Formats a log message",
      fun () ->
              Offset =
                  calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time({0,86400,0})) -
                  calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time({0,86400,0})),
              Timestamp = {0, 86400 + Offset, 0},
              Location = logi_location:new(c:pid(0,0,0), app, mod, 'fun', 10),
              Context = logi_context:new(test_log, Timestamp, info, Location, #{key => val}, #{}),
              Layout = logi_builtin_layout_simple:new(),
              ?assertEqual("1970-01-02 00:00:00.000 [info] nonode@nohost <0.0.0> mod:fun:10 [key=val] hello world\n",
                           lists:flatten(logi_layout:format(Context, "hello ~s", [world], Layout)))
      end}
    ].
