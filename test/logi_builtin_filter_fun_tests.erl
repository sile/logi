%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_filter_fun_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a `logi_builtin_filter_fun' filter",
      fun () ->
              Filter = logi_builtin_filter_fun:new(fun (_) -> true end),
              ?assert(logi_filter:is_filter(Filter))
      end},
     {"[ERORR] the argument is not a `filter_fun/0'",
      fun () ->
              ?assertError(badarg, logi_builtin_filter_fun:new(hoge)), % not a function
              ?assertError(badarg, logi_builtin_filter_fun:new(fun lists:map/2))  % not a `filter_fun/0' function
      end}
    ].

filter_test_() ->
    [
     {"Filters log messages (by the context)",
      fun () ->
              %% A level filter
              FilterFun = fun (Context) -> logi:severity_level(info) > logi:severity_level(logi_context:get_severity(Context)) end,
              Filter = logi_builtin_filter_fun:new(FilterFun),

              ?assertNot(logi_filter:apply(logi_context:new(test_log, debug), Filter)),
              ?assertNot(logi_filter:apply(logi_context:new(test_log, info), Filter)),
              ?assert(logi_filter:apply(logi_context:new(test_log, notice), Filter)),
              ?assert(logi_filter:apply(logi_context:new(test_log, alert), Filter))
      end}
    ].
