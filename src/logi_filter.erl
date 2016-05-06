%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Message Filter Behaviour
%%
%% A filter decides whether to allow a message be sent to the target channel.
%%
%% == NOTE ==
%% A filter should not raise exceptions when it's `filter/2' is called.
%%
%% If any exception is raised, the invocation of the log function will be aborted and
%% the exception will be propagated to the caller process.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% > Context0 = logi_context:new(sample_log, info).
%% > FilterFun = fun (C) -> not maps:get(discard, logi_context:get_metadata(C), false) end.
%% > Filter = logi_builtin_filter_fun:new(FilterFun).
%% > logi_filter:apply(Context0, Filter).
%% true
%% > Context1 = logi_context:from_map(maps:put(metadata, #{discard => true}, logi_context:to_map(Context0))).
%% > logi_filter:apply(Context1, Filter).
%% false
%% </pre>
%%
%% A more realistic example:
%% <pre lang="erlang">
%% > WriteFun = fun (_, Format, Data) -> io:format(Format ++ "\n", Data) end.
%% > {ok, _} = logi_channel:install_sink(logi_builtin_sink_fun:new(foo, WriteFun), info).
%%
%% > FilterFun = fun (C) -> not maps:get(discard, logi_context:get_metadata(C), false) end.
%% > Logger = logi:new([{filter, logi_builtin_filter_fun:new(FilterFun)}]).
%% > logi:save_as_default(Logger).
%%
%% > logi:info("hello world").
%% hello world
%%
%% > logi:info("hello world", [], [{metadata, #{discard => true}}]).
%% % No output: the log message was discarded by the filter
%% </pre>
%% @end
-module(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_filter/1]).
-export([get_module/1, get_state/1]).
-export([apply/2]).

-export_type([filter/0]).
-export_type([callback_module/0, state/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback filter(logi_context:context(), state()) -> boolean() | {boolean(), state()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque filter() :: {callback_module(), state()}
                  | callback_module().
%% An instance of `logi_filter' behaviour implementation module.

-type callback_module() :: module().
%% A module that implements the `logi_filter' behaviour.

-type state() :: term().
%% The value of the second arguemnt of the `filter/2' callback function.
%%
%% If the `filter()' does not have an explicit `state()', `undefined' will be passed instead.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Module, undefined)
-spec new(callback_module()) -> filter().
new(Module) -> new(Module, undefined).

%% @doc Creates a new filter instance
-spec new(callback_module(), state()) -> filter().
new(Module, State) ->
    _ = is_filter(Module) orelse error(badarg, [Module, State]),
    unsafe_new(Module, State).

%% @doc Returns `true' if `X' is a filter, `false' otherwise
-spec is_filter(X :: (filter() | term())) -> boolean().
is_filter({Module, _}) -> is_atom(Module) andalso is_filter(Module);
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
%%
%% This function returns `DoAllow' if the state of `Filter' is not changed, `{DoAllow, NewFilter}' otherwise.
-spec apply(logi_context:context(), Filter :: filter()) -> DoAllow | {DoAllow, NewFilter} when
      DoAllow   :: boolean(),
      NewFilter :: filter().
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
%% @doc Creates a new filter instance without validating the arguments
-spec unsafe_new(callback_module(), state()) -> filter().
unsafe_new(Module, undefined) -> Module;
unsafe_new(Module, State)     -> {Module, State}.
