%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Message Filter Behaviour
%%
%% A filter decides whether to allow or deny a message which send to the target channel.
%% TODO: doc
%%
%% TODO: filter shuold not raise exceptions
%%
%% TODO: more realistic example (logi:add_filter => logi:info)
%%
%% TODO: logi_builtin_filter_or, logi_builtin_filter_and, logi_builtin_filter_true, logi_builtin_filter_false
%%
%% <pre lang="erlang">
%% %%%
%% %%% Example
%% %%%
%% > Context = logi_context:new(sample_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}).
%% > FilterFun = fun (_, _, _) -> true end,
%% > Filter = logi_builtin_filter_fun:new(FilterFun),
%% > logi_filter:apply(Context, [], Filter).
%% true
%% </pre>
-module(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_filter/1]).
-export([get_module/1, get_state/1]).
-export([apply/2]).

-export_type([filter/0, filter/1]).
-export_type([callback_module/0, state/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback filter(logi_context:context(), state()) -> boolean() | {boolean(), state()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type filter() :: filter(state()).
%% An instance of `logi_filter' behaviour implementation module.

-opaque filter(State) :: {callback_module(), State}
                       | callback_module().
%% A specialized type of `filter/0'.
%% This may be useful for modules which want to annotate their own `State' type.

-type callback_module() :: module().
%% A module that implements the `logi_filter' behaviour.

-type state() :: term().
%% The value of the third arguemnt of the `filter/3' callback function.
%%
%% If the `filter()' does not have an explicit `state()', `undefined' will be passed instead.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Module, undefined)
-spec new(callback_module()) -> filter().
new(Module) -> new(Module, undefined).

%% @doc Creates a new filter instance
-spec new(callback_module(), State) -> filter(State) when State :: state().
new(Module, State) ->
    _ = is_filter(Module) orelse error(badarg, [Module, State]),
    unsafe_new(Module, State).

%% @doc Returns `true' if `X' is a filter, `false' otherwise
-spec is_filter(X :: (filter() | term())) -> boolean().
is_filter({Module, _}) -> is_filter(Module);
is_filter(Module)      -> is_atom(Module) andalso logi_utils:function_exported(Module, filter, 2).

%% @doc Gets the module of `Filter'
-spec get_module(Filter :: filter()) -> callback_module().
get_module(Module) when is_atom(Module) -> Module;
get_module({Module, _})                 -> Module.

%% @doc Gets the state of `Filter'
-spec get_state(Filter :: filter()) -> state().
get_state(Module) when is_atom(Module) -> undefined;
get_state({_, State})                  -> State.

%% @doc Applies `Filter'
-spec apply(logi_context:context(), Filter :: filter()) -> boolean() | {boolean(), filter()}.
apply(Context, Filter) ->
    Module = get_module(Filter),
    State0 = get_state(Filter),
    case Module:filter(Context, State0) of
        {Bool, State1} -> {Bool, unsafe_new(Module, State1)};
        Bool           -> Bool
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec unsafe_new(callback_module(), state()) -> filter().
unsafe_new(Module, undefined) -> Module;
unsafe_new(Module, State)     -> {Module, State}.
