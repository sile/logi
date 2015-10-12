%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Message Filter Behaviour
%%
%% TODO: doc
-module(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_filter/1]).
-export([get_module/1, get_state/1]).
-export([apply/3]).

-export_type([filter/0, filter/1]).
-export_type([callback_module/0, state/0]).
-export_type([option/0, options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback filter(logi_context:context(), options(), state()) -> boolean() | {boolean(), state()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type filter() :: filter(state()).
%% An instance of `logi_filter' behaviour implementation module.

-opaque filter() :: {callback_module(), state()}.
-type callback_module() :: module().
-type state() :: term().
-type options() :: [option()].
-type option() :: {Key::term(), Value::term()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(callback_module()) -> filter().
new(Module) -> new(Module, undefined).

-spec new(callback_module(), state()) -> filter().
new(Module, State) ->
    _ = is_atom(Module) orelse error(badarg, [Module, State]),
    {Module, State}.

-spec is_filter(term()) -> boolean().
is_filter({Module, _}) -> is_atom(Module);
is_filter(_)           -> false.

-spec apply(logi_context:context(), options(), filter()) -> {boolean(), filter()}.
apply(Context, Options, {M, S0}) ->
    {Bool, S1} = M:filter(Context, Options, S0),
    {Bool, {M, S1}}.
