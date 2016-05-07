%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sink Writer Behaviour
%%
%% A sink writer will write log messages to a destination sink process.
%%
%% The main purpose of writers is to write messages to some output devices (e.g., tty, file, socket).
%%
%% @end
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
-export([get_writee/1]).

-export_type([writer/0]).
-export_type([callback_module/0]).
-export_type([state/0]).
-export_type([written_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_context:context(), io:format(), logi_layout:data(), state()) -> written_data().
-callback get_writee(state()) -> pid() | undefined.

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque writer() :: {callback_module(), state()}.
%% A writer instance.

-type callback_module() :: module().
%% A module that implements the `logi_sink_writer' behaviour.

-type state() :: term().
%% The value of the fourth arguemnt of the `write/4' callback function.
%%
%% NOTE:
%% This value might be loaded from ETS every time when a log message is issued.
%% Therefore, very huge state can cause a performance problem.

-type written_data() :: logi_layout:formatted_data().
%% The data written to a sink

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
is_callback_module(X) ->
    (is_atom(X) andalso
     logi_utils:function_exported(X, write, 4) andalso
     logi_utils:function_exported(X, get_writee, 1)).

%% @doc Gets the module of `Writer'
-spec get_module(Writer :: writer()) -> callback_module().
get_module({Module, _}) ->
    Module.

%% @doc Gets the state of `Writer'
-spec get_state(Writer :: writer()) -> state().
get_state({_, State}) ->
    State.

%% @doc Writes a log message
%%
%% If it fails to write, an exception will be raised.
-spec write(logi_context:context(), io:format(), logi_layout:data(), Writer :: writer()) -> written_data().
write(Context, Format, Data, {Module, State}) ->
    Module:write(Context, Format, Data, State).

%% @doc Gets the writee process of log messages
%%
%% "writee" is the destination process of `written_data()' of {@link write/4}.
%%
%% If such process is dead or unknown, the function returns `undefined'.
%%
%% The result value might change on every call.
-spec get_writee(Writer :: writer()) -> pid() | undefined.
get_writee({Module, State}) ->
    Module:get_writee(State).
