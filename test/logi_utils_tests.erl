%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
is_timestamp_test_() ->
    [
     ?_assert(logi_utils:is_timestamp({1,1,1})),
     ?_assert(logi_utils:is_timestamp({0,0,0})),
     ?_assertNot(logi_utils:is_timestamp({-1,0,0})),
     ?_assertNot(logi_utils:is_timestamp(not_timestamp))
    ].

function_exported_test_() ->
    [
     ?_assert(logi_utils:function_exported(lists, map, 2)),
     ?_assertNot(logi_utils:function_exported(list__, map, 2)),
     ?_assertNot(logi_utils:function_exported(lists, map__, 2)),
     ?_assertNot(logi_utils:function_exported(lists, map, 0))
    ].
