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
-module(logi_backend_table).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
         delete/1,
         find_backend/2,
         register_backend/2
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
-spec register_backend(table(), logi:backend()) -> ok.
register_backend(Table, Backend) ->
    ok = add_backend(Table, Backend),
    ok = add_condition(Table, Backend),
    ok.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
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

-spec add_backend(table(), logi:backend()) -> ok.
add_backend(Table, Backend) ->
    true = ets:insert(Table, {{backend, logi_backend:get_id(Backend)}, Backend}),
    ok.

-spec target_severities(logi:log_level()) -> [logi:log_level()].
target_severities(Level) ->
    lists:dropwhile(fun (L) -> L =/= Level end, logi:log_levels()).
