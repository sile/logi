%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A logger process
%% @private
-module(logi_logger).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([register_appender/4]).
-export([which_appenders/1]).
         %% set_backend/3,
         %% delete_backend/2,
         %% find_backend/2,
         %% which_backends/1,
         %% select_backends/5,
         %% get_condition/2,
         %% set_condition/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%----------------------------------------------------------------------------------------------------------------------
-define(VALIDATE_AND_GET_LOGGER_PID(LoggerId, Args),
        case is_atom(LoggerId) of
            false -> error(badarg, Args);
            true  -> case whereis(LoggerId) of
                         undefined -> error({logger_is_not_running, LoggerId}, Args);
                         LoggerPid -> LoggerPid
                     end
        end).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          id    :: logi:logger_id(),
          table :: logi_appender_table:table()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a logger process
-spec start_link(logi:logger_id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id], []).

%% @doc Registers an appender
-spec register_appender(logi:logger_id(), logi_appender:id(), logi_appender:appender(), Options) -> Result when
      Options :: #{
        condition => todo,
        owner     => undefined | pid(),
        if_exists => error | ignore | supersede
       },
      Result :: {ok, OldAppender} | {error, Reason},
      OldAppender :: undefined | logi_appender:appender(),
      Reason :: {already_registered, logi_appender:appender()}.
register_appender(Id, AppenderId, Appender, Options) ->
    Args = [Id, AppenderId, Appender, Options],
    Defaults = #{condition => [], owner => undefined, if_exists => error},
    case maps:merge(Defaults, Options) of
        #{owner := X}     when X =/= undefined, not is_pid(X)                 -> error(badarg, Args);
        #{if_exists := X} when X =/= error, X =/= ignore, X =/= supersede     -> error(badarg, Args);
        #{condition := ConditionSpec, owner := Owner, if_exists := IfExists}  ->
            Pid = ?VALIDATE_AND_GET_LOGGER_PID(Id, Args),
            Condition = logi_condition:make(ConditionSpec),
            gen_server:call(Pid, {register_appender, {AppenderId, Appender, Condition, Owner, IfExists}})
    end.

%% @doc Returns a list of registered appenders
-spec which_appenders(logi:logger_id()) -> [logi_appender:id()].
which_appenders(Id) ->
    _ = ?VALIDATE_AND_GET_LOGGER_PID(Id, [Id]),
    logi_appender_table:which_appenders(Id).

%% %% @doc Sets the backend
%% %%
%% %% If a backend which have the same ID exists, it will be overwritten
%% -spec set_backend(logi:logger(), logi_backend:backend(), logi_condition:condition()) -> ok.
%% set_backend(ManagerRef, Backend, Condition) ->
%%     gen_server:call(ManagerRef, {set_backend, {Backend, Condition}}).

%% %% @doc Deletes the backend
%% %%
%% %% This functions always returns `ok'
%% -spec delete_backend(logi:logger(), logi_backend:id()) -> ok.
%% delete_backend(ManagerRef, BackendId) ->
%%     gen_server:cast(ManagerRef, {delete_backend, BackendId}).

%% %% @doc Finds a backend which have the ID `BackendId'
%% -spec find_backend(logi:logger(), logi_backend:id()) -> {ok, logi_backend:backend()} | error.
%% find_backend(ManagerRef, BackendId) ->
%%     logi_backend_table:find_backend(ManagerRef, BackendId).

%% %% @doc Selects backends which satisfies the specified condition
%% -spec select_backends(logi:logger(), logi:severity(), logi_location:location(), logi:headers(), logi:metadata()) -> [logi_backend:backend()].
%% select_backends(ManagerRef, Severity, Location, Headers, MetaData) ->
%%     logi_backend_table:select_backends(ManagerRef, Severity, Location, Headers, MetaData).

%% %% @doc Gets the output condition of the backend
%% -spec get_condition(logi:logger(), logi_backend:id()) -> {ok, logi_condition:condition()} | {error, not_found}.
%% get_condition(ManagerRef, BackendId) ->
%%     gen_server:call(ManagerRef, {get_condition, BackendId}).

%% %% @doc Sets the output condition of the backend
%% -spec set_condition(logi:logger(), logi_backend:id(), logi_condition:condition()) -> ok | {error, not_found}.
%% set_condition(ManagerRef, BackendId, Condition) ->
%%     gen_server:call(ManagerRef, {set_condition, {BackendId, Condition}}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Id]) ->
    _ = process_flag(trap_exit, true),
    State =
        #?STATE{
            id    = Id,
            table = logi_appender_table:new(Id)
           },
    {ok, State}.

%% @private
handle_call(_, _, State)                        -> {noreply, State}.

%% @private
handle_cast(_, State)                     -> {noreply, State}.

%% @private
handle_info(_, State) ->
    %% TODO: logi:warning
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
