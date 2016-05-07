%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
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

is_non_neg_integer_test_() ->
    [
     ?_assert(logi_utils:is_non_neg_integer(0)),
     ?_assert(logi_utils:is_non_neg_integer(1)),
     ?_assertNot(logi_utils:is_non_neg_integer(-1)),
     ?_assertNot(logi_utils:is_non_neg_integer(1.0)),
     ?_assertNot(logi_utils:is_non_neg_integer(atom))
    ].

is_pos_integer_test_() ->
    [
     ?_assert(logi_utils:is_pos_integer(1)),
     ?_assertNot(logi_utils:is_pos_integer(0)),
     ?_assertNot(logi_utils:is_pos_integer(-1)),
     ?_assertNot(logi_utils:is_pos_integer(1.0)),
     ?_assertNot(logi_utils:is_pos_integer(atom))
    ].

function_exported_test_() ->
    [
     ?_assert(logi_utils:function_exported(lists, map, 2)),
     ?_assertNot(logi_utils:function_exported(list__, map, 2)),
     ?_assertNot(logi_utils:function_exported(lists, map__, 2)),
     ?_assertNot(logi_utils:function_exported(lists, map, 0))
    ].
