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
-define(STATE, ?MODULE).

-record(?STATE,
        {
          id                   :: logi:logger_id(),
          table                :: logi_backend_table:table(),
          backend_to_condition :: gb_trees:tree(logi_backend:id(), logi_condition:condition())
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a logger process
-spec start_link(logi:logger_id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id], []).

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

%% %% @doc Returns existing backends
%% -spec which_backends(logi:logger()) -> [logi_backend:backend()].
%% which_backends(ManagerRef) ->
%%     logi_backend_table:which_backends(ManagerRef).

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
            id                   = Id,
            table                = logi_backend_table:new(Id),
            backend_to_condition = gb_trees:empty()
           },
    {ok, State}.

%% @private
handle_call(get_id, _From, State)               -> {reply, State#?STATE.id, State};
handle_call({get_condition, Arg}, _From, State) -> handle_get_condition(Arg, State);
handle_call({set_condition, Arg}, _From, State) -> handle_set_condition(Arg, State);
handle_call({set_backend, Arg},   _From, State) -> handle_set_backend(Arg, State);
handle_call(_, _, State)                        -> {noreply, State}.

%% @private
handle_cast({delete_backend, Arg}, State) -> handle_delete_backend(Arg, State);
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
-spec handle_set_backend({logi_backend:backend(), logi_condition:condition()}, #?STATE{}) -> {reply, ok, #?STATE{}}.
handle_set_backend({Backend, Condition}, State) ->
    ok = logi_backend_table:register_backend(State#?STATE.table, Condition, Backend),
    BackendToCondition = gb_trees:enter(logi_backend:get_id(Backend), Condition, State#?STATE.backend_to_condition),
    {reply, ok, State#?STATE{backend_to_condition = BackendToCondition}}.

-spec handle_delete_backend(logi_backend:id(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_delete_backend(BackendId, State) ->
    ok = logi_backend_table:deregister_backend(State#?STATE.table, BackendId),
    BackendToCondition = gb_trees:delete_any(BackendId, State#?STATE.backend_to_condition),
    {noreply, State#?STATE{backend_to_condition = BackendToCondition}}.

-spec handle_get_condition(logi_backend:id(), #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: {ok, logi_condition:condition()} | {error, not_found}.
handle_get_condition(BackendId, State) ->
    case gb_trees:lookup(BackendId, State#?STATE.backend_to_condition) of
        none               -> {reply, {error, not_found}, State};
        {value, Condition} -> {reply, {ok, Condition}, State}
    end.

-spec handle_set_condition({logi_backend:id(), logi_condition:condition()}, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: ok | {error, not_found}.
handle_set_condition({BackendId, Condition}, State) ->
    case logi_backend_table:find_backend(State#?STATE.table, BackendId) of
        error         -> {reply, {error, not_found}, State};
        {ok, Backend} -> handle_set_backend({Backend, Condition}, State)
    end.
