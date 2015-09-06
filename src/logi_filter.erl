%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_filter/1]).
-export([to_map/1, from_map/1]).
-export([apply/3]).

-export_type([filter/0]).
-export_type([callback_module/0, state/0]).
-export_type([option/0, options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback filter(logi_context:context(), options(), state()) -> {boolean(), state()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------

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

-spec to_map(filter()) -> #{module => callback_module(), state => state()}.
to_map({Module, State}) when is_atom(Module) -> #{module => Module, state => State};
to_map(X)                                    -> error(badarg, [X]).

-spec from_map(Map) -> filter() when
      Map :: #{module => callback_module(), state => state()}.
from_map(#{module := Module, state := State}) when is_atom(Module) -> new(Module, State);
from_map(#{module := Module})                 when is_atom(Module) -> new(Module);
from_map(X)                                                        -> error(badarg, [X]).

-spec apply(logi_context:context(), options(), filter()) -> {boolean(), filter()}.
apply(Context, Options, {M, S0}) ->
    {Bool, S1} = M:filter(Context, Options, S0),
    {Bool, {M, S1}}.
