%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc internal header file

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(ASSERT_PRED_FUN(M, F, Arg),
        case M:F(Arg) of
            true  -> true;
            false -> error({assertion_failed, {mfarsg, {M, F, [Arg]}}})
        end).

-define(ASSERT_PRED_FUN(F, Arg),
        case F(Arg) of
            true  -> true;
            false -> error({assertion_failed, {mfarsg, {?MODULE, F, Arg}}})
        end).
