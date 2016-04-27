-module(logi_sink_writer).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2]).
-export([is_writer/1]).
-export([is_callback_module/1]).
-export([get_module/1]).
-export([get_state/1]).
-export([write/4]).

-export_type([writer/0]).
-export_type([callback_module/0]).
-export_type([state/0]).
-export_type([written_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_context:context(), io:format(), logi_layout:data(), state()) -> written_data().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque writer() :: {callback_module(), state()}. % TODO: client(?)
%% A writer instance.

-type callback_module() :: module().
%% A module that implements the `logi_sink' behaviour.

-type state() :: term(). % TODO: client_state (?)
%% The value of the fourth arguemnt of the `write/4' callback function.
%%
%% NOTE:
%% This value will be loaded from ETS every time the `write/4' is called.
%% Therefore, very huge data can cause a performance issue.

-type written_data() :: logi_layout:formatted_data().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new writer instance
-spec new(callback_module(), state()) -> writer().
new(Module, State) ->
    _ = is_callback_module(Module) orelse error(badarg, [Module, State]),
    {Module, State}.

%% @doc Returns `true' if `X' is a writer instance, otherwise `false'
-spec is_writer(X :: (writer() | term())) -> boolean().
is_writer({Module, _}) -> is_callback_module(Module);
is_writer(_)           -> false.

%% @doc Returns `true' if `X' is a module which implements the `writer' behaviour, otherwise `false'
-spec is_callback_module(X :: (callback_module() | term())) -> boolean().
is_callback_module(X) -> (is_atom(X) andalso logi_utils:function_exported(X, write, 4)).

-spec get_module(writer()) -> callback_module().
get_module({Module, _}) -> Module.

-spec get_state(writer()) -> state().
get_state({_, State}) -> State.

%% @doc Writes a log message
%%
%% If it fails to write, an exception will be raised.
-spec write(logi_context:context(), io:format(), logi_layout:data(), writer()) -> written_data().
write(Context, Format, Data, {Module, State}) ->
    Module:write(Context, Format, Data, State).
