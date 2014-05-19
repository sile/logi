%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc logiのバックエンド管理用ETSに対する簡便なインタフェースを提供するモジュール
%%
%% ETSに格納されるデータの構造:
%% ```
%% %% バックエンドID => バックエンド
%% {{backend, logi:backend_id()}, logi:backend()}
%%
%% %% severity => 条件付きバックエンド
%% {{severity, logi:severity()}, [logi:conditional_backend()]}
%% '''
%%
%% TODO: 諸々最適化 (constraint() で none が存在するバックエンドは、それ以外の制約は除去する、等)
-module(logi_backend_table).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
         delete/1,
         find_backend/2,
         register_backend/2,
         deregister_backend/2,
         select_backends/3
        ]).

-export_type([
              table/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type table() :: ets:tab().

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc 新しいバックエンド群管理用ETSテーブルを作成する
-spec new(atom()) -> ets:tab().
new(TableName) ->
    ets:new(TableName, [set, protected, {read_concurrency, true}, named_table]).

%% @doc バックエンド群管理用ETSテーブルを破棄する
-spec delete(table()) -> ok.
delete(Table) ->
    true = ets:delete(Table),
    ok.

%% @doc バックエンドを検索する
-spec find_backend(table(), logi:backend_id()) -> {ok, logi:backend()} | error.
find_backend(Table, BackendId) ->
    case ets:lookup(Table, {backend, BackendId}) of
        []             -> error;
        [{_, Backend}] -> {ok, Backend}
    end.

%% @doc バックエンドを登録する
%%
%% 既に同じIDに紐づくバックエンドが存在する場合は、内容が更新される
-spec register_backend(table(), logi:backend()) -> ok.
register_backend(Table, Backend) ->
    ok = add_backend(Table, Backend),
    ok = delete_condition(Table, Backend), % 前回の結果を消去 (XXX: これだと更新時にログが出力されない瞬間ができてしまう)
    ok = add_condition(Table, Backend),
    ok.

%% @doc バックエンドの登録を解除する
%%
%% IDに対応するバックエンドが存在しない場合は、単に無視される
-spec deregister_backend(table(), logi:backend_id()) -> ok.
deregister_backend(Table, BackendId) ->
    case find_backend(Table, BackendId) of
        error         -> ok;
        {ok, Backend} ->
            ok = delete_backend(Table, Backend),
            ok = delete_condition(Table, Backend),
            ok
    end.

%% @doc 条件に一致するバックエンド群を選択する
-spec select_backends(table(), logi:severity(), [logi:metadata_entry()]) -> [logi:backend()].
select_backends(Table, Severity, MetaData) ->
    BackendIds = [ BackendId || {ConditionClause, BackendId} <- load_conditional_backends(Table, Severity),
                                logi_condition:is_satisfied(ConditionClause, MetaData)],
    [load_backend(Table, BackendId) || BackendId <- lists:usort(BackendIds)].

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec add_backend(table(), logi:backend()) -> ok.
add_backend(Table, Backend) ->
    true = ets:insert(Table, {{backend, logi_backend:get_id(Backend)}, Backend}),
    ok.

-spec delete_backend(table(), logi:backend()) -> ok.
delete_backend(Table, Backend) ->
    true = ets:delete(Table, {backend, logi_backend:get_id(Backend)}),
    ok.

-spec add_condition(table(), logi:backend()) -> ok.
add_condition(Table, Backend) ->
    Condition = logi_backend:get_condition(Backend),
    add_condition_clauses(Table, logi_backend:get_id(Backend), logi_condition:get_normalized_spec(Condition)).

-spec add_condition_clauses(table(), logi:backend_id(), [logi_condition:condition_clause()]) -> ok.
add_condition_clauses(_Table, _BackendId, [])                         -> ok;
add_condition_clauses(Table, BackendId, [{Level, Constraint} | Rest]) ->
    ok = lists:foreach(
           fun (Severity) ->
                   Backends0 = load_conditional_backends(Table, Severity),
                   Backends1 = lists:umerge([{Constraint, BackendId}], Backends0),
                   save_conditional_backends(Table, Severity, Backends1)
           end,
           target_severities(Level)),
    add_condition_clauses(Table, BackendId, Rest).

-spec delete_condition(table(), logi:backend()) -> ok.
delete_condition(Table, Backend) ->
    DeleteId = logi_backend:get_id(Backend),
    lists:foreach(
      fun (Severity) ->
              Backends0 = load_conditional_backends(Table, Severity),
              Backends1 = lists:filter(fun ({_, BackendId}) -> BackendId =/= DeleteId end, Backends0),
              save_conditional_backends(Table, Severity, Backends1)
      end,
      logi:log_levels()).

-spec load_conditional_backends(table(), logi:severity()) -> [{logi:condition_clause(), logi:backend_id()}].
load_conditional_backends(Table, Severity) ->
    case ets:lookup(Table, {severity, Severity}) of
        []              -> [];
        [{_, Backends}] -> Backends
    end.

-spec save_conditional_backends(table(), logi:severity(), [{logi:condition_clause(), logi:backend_id()}]) -> ok.
save_conditional_backends(Table, Severity, Backends) ->
    true = ets:insert(Table, {{severity, Severity}, Backends}),
    ok.

-spec load_backend(table(), logi:backend_id()) -> logi:backend().
load_backend(Table, BackendId) ->
    [{_, Backend}] = ets:lookup(Table, {backend, BackendId}),
    Backend.

-spec target_severities(logi:log_level()) -> [logi:log_level()].
target_severities(Level) ->
    lists:dropwhile(fun (L) -> L =/= Level end, logi:log_levels()).
