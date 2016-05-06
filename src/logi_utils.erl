%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Miscellaneous utility functions
%% @private
%% @end
-module(logi_utils).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([is_timestamp/1]).
-export([is_non_neg_integer/1]).
-export([is_pos_integer/1]).
-export([function_exported/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Returns `true' if `X' is a timestamp, `false' otherwise
-spec is_timestamp(X :: (erlang:timestamp() | term())) -> boolean().
is_timestamp({A, B, C}) when is_integer(A), A >= 0,
                             is_integer(B), B >= 0,
                             is_integer(C), C >= 0 ->
    true;
is_timestamp(_) ->
    false.

%% @doc Returns `true' if `X' is a non negative integer, `false' otherwise
-spec is_non_neg_integer(X :: (non_neg_integer() | term())) -> boolean().
is_non_neg_integer(X) ->
    is_integer(X) andalso X >= 0.

%% @doc Returns `true' if `X' is a positive integer, `false' otherwise
-spec is_pos_integer(X :: (non_neg_integer() | term())) -> boolean().
is_pos_integer(X) ->
    is_integer(X) andalso X > 0.

%% @doc Equivalent to {@link erlang:function_exported/3} except `Module' will be loaded if it has not been loaded
-spec function_exported(module(), atom(), arity()) -> boolean().
function_exported(Module, Function, Arity) ->
    _ = is_atom(Module) orelse error(badarg, [Module, Function, Arity]),
    _ = is_atom(Function) orelse error(badarg, [Module, Function, Arity]),
    _ = (is_integer(Arity) andalso Arity >= 0) orelse error(badarg, [Module, Function, Arity]),
    _ = code:is_loaded(Module) =/= false orelse code:load_file(Module),
    erlang:function_exported(Module, Function, Arity).
