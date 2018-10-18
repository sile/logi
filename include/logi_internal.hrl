%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc internal header file

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new try/catch syntax or not.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-else.
-define(CAPTURE_STACKTRACE, ).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-endif.
