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
         set_backend/3,
         delete_backend/2,
         find_backend/2,
         which_backends/1,
         select_backends/4,
         get_condition/2,
         set_condition/3
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
          id                   :: logi:logger(),
          table                :: logi_backend_table:table(),
          backend_to_condition :: gb_tree() % logi_backend:id() => logi_condition:condition()
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
-spec stop(logi:logger()) -> ok.
stop(ManagerRef) ->
    gen_server:cast(ManagerRef, stop).

%% @doc マネージャのIDを取得する
-spec get_id(pid() | logi:logger()) -> logi:logger().
get_id(ManagerRef) ->
    gen_server:call(ManagerRef, get_id).
    
%% @doc バックエンドを設定する
-spec set_backend(logi:logger(), logi_backend:backend(), logi_condition:condition()) -> ok.
set_backend(ManagerRef, Backend, Condition) ->
    gen_server:call(ManagerRef, {set_backend, {Backend, Condition}}).

%% @doc バックエンドを削除する
-spec delete_backend(logi:logger(), logi:backend_id()) -> ok.
delete_backend(ManagerRef, BackendId) ->
    gen_server:cast(ManagerRef, {delete_backend, BackendId}).

%% @doc IDに対応するバックエンドを検索する
-spec find_backend(logi:logger(), logi:backend_id()) -> {ok, logi:backend()} | error.
find_backend(ManagerRef, BackendId) ->
    logi_backend_table:find_backend(ManagerRef, BackendId).

%% @doc バックエンドを一覧を返す
-spec which_backends(logi:logger()) -> [logi:backend()].
which_backends(ManagerRef) ->
    logi_backend_table:which_backends(ManagerRef).

%% @doc 条件に合致するバックエンド群を選択する
-spec select_backends(logi:logger(), logi:severity(), logi:location(), logi:metadata()) -> [logi:backend()].
select_backends(ManagerRef, Severity, Location, MetaData) ->
    logi_backend_table:select_backends(ManagerRef, Severity, Location, MetaData).

%% @doc バックエンドの出力条件を取得する
-spec get_condition(logi:logger(), logi_backend:id()) -> {ok, logi:condition()} | {error, not_found}.
get_condition(ManagerRef, BackendId) ->
    gen_server:call(ManagerRef, {get_condition, BackendId}).

%% @doc バックエンドの出力条件を設定する
-spec set_condition(logi:logger(), logi_backend:id(), logi_condition:condition()) -> ok | {error, not_found}.
set_condition(ManagerRef, BackendId, Condition) ->
    gen_server:call(ManagerRef, {set_condition, {BackendId, Condition}}).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init([Name]) ->
    _ = process_flag(trap_exit, true),
    State =
        #state{
           id                   = Name,
           table                = logi_backend_table:new(Name),
           backend_to_condition = gb_trees:empty()
          },
    {ok, State}.

%% @private
handle_call(get_id, _From, State)               -> {reply, State#state.id, State};
handle_call({get_condition, Arg}, _From, State) -> {reply, do_get_condition(Arg, State), State};
handle_call({set_condition, Arg}, _From, State) ->
    State1 = do_set_condition(Arg, State),
    {reply, ok, State1};
handle_call({set_backend, Arg}, _From, State) ->
    {Result, State1} = do_set_backend(Arg, State),
    {reply, Result, State1};
handle_call(_, _, State) ->
    {noreply, State}.

%% @private
handle_cast({delete_backend, Arg}, State) ->
    State1 = do_delete_backend(Arg, State),
    {noreply, State1};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.
    
%% @private
handle_info({'EXIT', Pid, _}, State) ->
    State1 = do_delete_backends_by_pid(Pid, State),
    {noreply, State1};
handle_info(_, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec do_set_backend({logi_backend:backend(), logi_condition:condition()}, #state{}) -> #state{}.
do_set_backend({Backend, Condition}, State) ->
    %% 名前付きプロセスが指定された場合は、たとえ一時的に死んだとしてもそのうち再起動することが期待されるので、
    %% linkによる死活監視は行わない
    ok = logi_util_process:link_if_pid(logi_backend:get_process(Backend)),

    ok = logi_backend_table:register_backend(State#state.table, Condition, Backend),
    BackendToCondition = gb_trees:enter(logi_backend:get_id(Backend), Condition, State#state.backend_to_condition),
    State#state{backend_to_condition = BackendToCondition}.

-spec do_delete_backend(logi:backend_id(), #state{}) -> #state{}.
do_delete_backend(BackendId, State) ->
    case logi_backend_table:find_backend(State#state.table, BackendId) of
        error   -> ok;
        {ok, _} ->
            ok = logi_backend_table:deregister_backend(State#state.table, BackendId),
            BackendToCondition = gb_trees:delete_any(BackendId, State#state.backend_to_condition),
            State#state{backend_to_condition = BackendToCondition}
    end.

-spec do_delete_backends_by_pid(pid(), #state{}) -> #state{}.
do_delete_backends_by_pid(Pid, State) ->
    lists:foldl(
      fun (B, AccState) ->
              case logi_backend:get_process(B) of
                  Pid ->
                      BackendId = logi_backend:get_id(B),
                      ok = logi_backend_table:deregister_backend(State#state.table, BackendId),
                      BackendToCondition = gb_trees:delete_any(BackendId, AccState#state.backend_to_condition),
                      AccState#state{backend_to_condition = BackendToCondition};
                  _   -> AccState
              end
      end,
      State,
      logi_backend_table:which_backends(State#state.table)).

-spec do_get_condition(logi_backend:id(), #state{}) -> {ok, logi_condition:condition()} | {error, not_found}.
do_get_condition(BackendId, State) ->
    case gb_trees:lookup(BackendId, State#state.backend_to_condition) of
        none               -> {error, not_found};
        {value, Condition} -> {ok, Condition}
    end.

-spec do_set_condition({logi_backend:id(), logi_condition:condition()}, #state{}) -> {Result, #state{}} when
      Result :: ok | {error, not_found}.
do_set_condition({BackendId, Condition}, State) ->
    case logi_backend_table:find_backend(State#state.table, BackendId) of
        error         -> {{error, not_found}, State};
        {ok, Backend} -> {ok, do_set_backend({Backend, Condition}, State)}
    end.
