%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_restart_strategy).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_strategy/1]).
-export([get_module/1, get_state/1]).
-export([next/1]).

-export_type([strategy/0]).
-export_type([callback_module/0]).
-export_type([state/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback next(state()) -> {ok, timeout(), state()} |
                           {stop, term()} |
                           {uninstall_sink, term()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque strategy() :: {callback_module(), state()}.

-type callback_module() :: module().
-type state() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(callback_module()) -> strategy().
new(Module) -> new(Module, undefined).

-spec new(callback_module(), state()) -> strategy().
new(Module, State) ->
    _ = is_strategy(Module) orelse error(badarg, [Module, State]),
    {Module, State}.

-spec is_strategy(strategy() | term()) -> boolean().
is_strategy({Module, _}) -> is_atom(Module) andalso is_strategy(Module);
is_strategy(Module)      -> is_atom(Module) andalso logi_utils:function_exported(Module, next, 1).

-spec get_module(strategy()) -> callback_module().
get_module({Module, _}) -> Module.

-spec get_state(strategy()) -> state().
get_state({_, State}) -> State.

-spec next(strategy()) -> {ok, timeout(), strategy()} |
                          {stop, term()} |
                          {uninstall_sink, term()}.
next({Module, State0}) ->
    case Module:next(State0) of
        {ok, Timeout, State1} -> {ok, Timeout, {Module, State1}};
        Other                 -> Other
    end.
