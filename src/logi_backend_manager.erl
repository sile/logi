%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc logiのバックエンドを管理するためのモジュール
%% @private
-module(logi_backend_manager).

-behaviour(gen_server).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start_link/1,
         stop/1,
         get_id/1,
         add_backend/2,
         delete_backend/2,
         find_backend/2,
         which_backends/1,
         select_backends/4,
         update_backend/2
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
          id    :: logi:backend_manager_id(),
          table :: logi_backend_table:table()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc マネージャを起動する
-spec start_link(atom()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

%% @doc マネージャを停止する
-spec stop(logi:backend_manager_id()) -> ok.
stop(ManagerId) ->
    gen_server:cast(ManagerId, stop).

%% @doc マネージャのIDを取得する
-spec get_id(pid() | logi:backend_manager_id()) -> logi:backend_manager_id().
get_id(ManagerRef) ->
    gen_server:call(ManagerRef, get_id).
    
%% @doc バックエンドを追加する
-spec add_backend(logi:backend_manager_ref(), logi:backend()) -> ok | {error, Reason} when
      Reason :: {already_exists, logi:backend()}.
add_backend(ManagerRef, Backend) ->
    gen_server:call(ManagerRef, {add_backend, Backend}).

%% @doc バックエンドを更新する
-spec update_backend(logi:backend_manager_ref(), logi:backend()) -> ok | {error, not_found}.
update_backend(ManagerRef, Backend) ->
    gen_server:call(ManagerRef, {update_backend, Backend}).

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
-spec select_backends(logi:backend_manager_ref(), logi:severity(), logi:location(), logi:metadata()) -> [logi:backend()].
select_backends(ManagerRef, Severity, Location, MetaData) ->
    logi_backend_table:select_backends(ManagerRef, Severity, Location, MetaData).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init([Name]) ->
    _ = process_flag(trap_exit, true),
    State =
        #state{
           id    = Name,
           table = logi_backend_table:new(Name)
          },
    {ok, State}.

%% @private
handle_call({add_backend, Arg},    _From, State) -> {reply, do_add_backend(Arg, State), State};
handle_call({delete_backend, Arg}, _From, State) -> {reply, do_delete_backend(Arg, State), State};
handle_call({update_backend, Arg}, _From, State) -> {reply, do_update_backend(Arg, State), State};
handle_call(get_id, _From, State)                -> {reply, State#state.id, State};
handle_call(_, _, State) ->
    %% TODO: log
    {noreply, State}.

%% @private
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    %% TODO: log
    {noreply, State}.
    
%% @private
handle_info({'EXIT', Pid, _}, State) ->
    ok = do_delete_backends_by_ref(Pid, State),
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
-spec do_add_backend(logi:backend(), #state{}) -> ok | {error, Reason} when
      Reason :: {already_exists, logi:backend()}.
do_add_backend(Backend, State) ->
    #state{table = Table} = State,
    case logi_backend_table:find_backend(Table, logi_backend:get_id(Backend)) of
        {ok, ExistingBackend} -> {error, {already_exists, ExistingBackend}};
        error                 ->
            ok = logi_util_process:link_if_pid(logi_backend:get_ref(Backend)),
            ok = logi_backend_table:register_backend(Table, Backend)
    end.

-spec do_delete_backend(logi:backend_id(), #state{}) -> ok | {error, not_found}.
do_delete_backend(BackendId, State) ->
    #state{table = Table} = State,
    case logi_backend_table:find_backend(Table, BackendId) of
        error   -> {error, not_found};
        {ok, _} -> logi_backend_table:deregister_backend(Table, BackendId) % TODO: unlink process
    end.

-spec do_update_backend(logi:backend(), #state{}) -> ok | {error, not_found}.
do_update_backend(Backend, State) ->
    case logi_backend_table:find_backend(State#state.table, logi_backend:get_id(Backend)) of
        error   -> {error, not_found};
        {ok, _} -> logi_backend_table:register_backend(State#state.table, Backend)
    end.

-spec do_delete_backends_by_ref(pid(), #state{}) -> ok.
do_delete_backends_by_ref(Pid, State) ->
    lists:foreach(
      fun (B) ->
              case logi_backend:get_ref(B) of
                  Pid -> logi_backend_table:deregister_backend(State#state.table, logi_backend:get_id(B));
                  _   -> ok
              end
      end,
      logi_backend_table:which_backends(State#state.table)).
