%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc logiのバックエンドを管理するためのモジュール
-module(logi_backend_manager).

-behaviour(gen_server).
-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start_link/1,
         add_backend/2,
         delete_backend/2,
         find_backend/2,
         which_backends/1,
         select_backends/3,
         update_backend_condition_spec/3,
         update_backend_options/3
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          name   :: atom(),
          parent :: pid(),
          table  :: logi_backend_table:table()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc 名前付きのマネージャを起動する
-spec start_link(atom()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, self()], []).

%% @doc バックエンドを追加する
-spec add_backend(logi:backend_manager_ref(), #logi_backend{}) -> ok | {error, Reason} when
      Reason :: {already_exists, #logi_backend{}}.
add_backend(ManagerRef, Backend) ->
    gen_server:call(ManagerRef, {add_backend, Backend}).

%% @doc バックエンドを削除する
-spec delete_backend(logi:backend_manager_ref(), logi:backend_id()) -> ok | {error, not_found}.
delete_backend(ManagerRef, BackendId) ->
    gen_server:call(ManagerRef, {delete_backend, BackendId}).

%% @doc IDに対応するバックエンドを検索する
-spec find_backend(logi:backend_manager_ref(), logi:backend_id()) -> {ok, logi:backend()} | error.
find_backend(ManagerRef, BackendId) ->
    logi_backend_table:find_backend(ManagerRef, BackendId).

%% @doc バックエンドを一覧を返す
-spec which_backends(logi:backend_manager_ref()) -> [logi:backend()].
which_backends(ManagerRef) ->
    logi_backend_table:which_backends(ManagerRef).

%% @doc 条件に合致するバックエンド群を選択する
-spec select_backends(logi:backend_manager_ref(), logi:severity(), logi:conditions()) -> [logi:backend()].
select_backends(ManagerRef, Severity, Conditions) ->
    logi_backend_table:select_backends(ManagerRef, Severity, Conditions).

-spec update_backend_condition_spec(logi:backend_manager_ref(), logi:backend_id(), logi:condition_spec()) ->
                                           ok | {error, not_found}.
update_backend_condition_spec(ManagerRef, BackendId, ConditionSpec) ->
    gen_server:call(ManagerRef, {update_backend_condition_spec, {BackendId, ConditionSpec}}).

-spec update_backend_options(logi:backend_manager_ref(), logi:backend_id(), logi:condition_spec()) ->
                                    ok | {error, not_found}.
update_backend_options(ManagerRef, BackendId, BackendOptions) ->
    gen_server:call(ManagerRef, {update_backend_options, {BackendId, BackendOptions}}).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init([Name, Parent]) ->
    _ = process_flag(trap_exit, true),
    State =
        #state{
           name   = Name,
           parent = Parent,
           table  = logi_backend_table:table(Name)
          },
    {ok, State}.

%% @private
handle_call({add_backend, Arg}, _From, State)    -> {reply, do_add_backend(Arg, State), State};
handle_call({delete_backend, Arg}, _From, State) -> {reply, do_delete_backend(Arg, State), State};
handle_call({update_backend_condition_spec, Arg}, _From, State) ->
    {reply, do_update_backend_condition_spec(Arg, State), State};
handle_call({update_backend_options, Arg}, _From, State) ->
    {reply, do_update_backend_options(Arg, State), State};
handle_call(_, _, State) ->
    %% TODO: log
    {noreply, State}.

%% @private
handle_cast(_, State) ->
    %% TODO: log
    {noreply, State}.
    
%% @private
%% TODO: handling of 'EXIT'
handle_info({'EXIT', Pid, Reason}, State = #state{parent = Pid}) ->
    {stop, Reason, State};
handle_info({'EXIT', Pid, _}, State) ->
    ok = logi_backend_table:delete_backends_by_ref(State#state.table, Pid),
    {noreply, State};
handle_info(_, State) ->
    %% TODO: log
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    %% TODO: log
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    %% TODO: log
    {ok, State}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec do_add_backend(#logi_backend{}, #state{}) -> ok | {error, Reason} when
      Reason :: {already_exists, #logi_backend{}}.
do_add_backend(Backend, State) ->
    #state{table = Table} = State,
    case logi_backend_table:find_backend(Table, Backend#logi_backend.id) of
        {ok, ExistingBackend} -> {error, {already_exists, ExistingBackend}};
        error                 ->
            ok = logi_util_process:link_if_pid(Backend#logi_backend.ref),
            ok = logi_backend_table:register_backend(Table, Backend)
    end.

-spec do_delete_backend(logi:backend_id(), #state{}) -> ok | {error, not_found}.
do_delete_backend(BackendId, State) ->
    #state{table = Table} = State,
    case logi_backend_table:find_backend(Table, BackendId) of
        error         -> {error, not_found};
        {ok, Backend} -> logi_backend_table:deregister_backend(Table, Backend) % TODO: unlink process
    end.

-spec do_update_backend_options({logi:backend_id(), logi:backend_options()}, #state{}) -> ok | {error, not_found}.
do_update_backend_options({BackendId, BackendOptions}, State) ->
    case logi_backend_table:find_backend(State#state.table, BackendId) of
        error   -> {error, not_found};
        {ok, _} -> logi_backend_table:update_backend_options(State#state.table, BackendId, BackendOptions)
    end.

-spec do_update_backend_condition_spec({logi:backend_id(), logi:condition_spec()}, #state{}) -> ok | {error, not_found}.
do_update_backend_condition_spec({BackendId, ConditionSpec}, State) ->
    case logi_backend_table:find_backend(State#state.table, BackendId) of
        error   -> {error, not_found};
        {ok, _} -> logi_backend_table:update_backend_condition_spec(State#state.table, BackendId, ConditionSpec)
    end.
