%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc logiのバックエンド管理用ETSに対する簡便なインタフェースを提供するモジュール
%%
%% ETSに格納されるデータの構造:
%% ```
%% %% バックエンドID => バックエンド
%% {{backend, logi_backend:id()}, logi_backend:backend()}
%%
%% %% severity => 条件付きバックエンド
%% {{severity, logi:severity()}, [{logi_condition:condition_clause(), logi_backend:id()}]}
%% '''
%%
%% TODO: 諸々最適化 (constraint() で none が存在するバックエンドは、それ以外の制約は除去する、等)
%% @private
-module(logi_backend_table).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
         delete/1,
         find_backend/2,
         register_backend/3,
         deregister_backend/2,
         select_backends/5,
         which_backends/1
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
-spec find_backend(table(), logi_backend:id()) -> {ok, logi_backend:backend()} | error.
find_backend(Table, BackendId) ->
    case ets:lookup(Table, {backend, BackendId}) of
        []             -> error;
        [{_, Backend}] -> {ok, Backend}
    end.

%% @doc バックエンドを登録する
%%
%% 既に同じIDに紐づくバックエンドが存在する場合は、内容が更新される
-spec register_backend(table(), logi_condition:condition(), logi_backend:backend()) -> ok.
register_backend(Table, Condition, Backend) ->
    ok = add_backend(Table, Backend),
    ok = delete_condition(Table, Backend), % 前回の結果を消去 (XXX: これだと更新時にログが出力されない瞬間ができてしまう)
    ok = add_condition(Table, Condition, Backend),
    ok.

%% @doc バックエンドの登録を解除する
%%
%% IDに対応するバックエンドが存在しない場合は、単に無視される
-spec deregister_backend(table(), logi_backend:id()) -> ok.
deregister_backend(Table, BackendId) ->
    case find_backend(Table, BackendId) of
        error         -> ok;
        {ok, Backend} ->
            ok = delete_backend(Table, Backend),
            ok = delete_condition(Table, Backend),
            ok
    end.

%% @doc 条件に一致するバックエンド群を選択する
-spec select_backends(table(), logi:severity(), logi_location:location(), logi:headers(), logi:metadata()) -> [logi_backend:backend()].
select_backends(Table, Severity, _Location, _Headers, _MetaData) ->
    %% TODO: 整理
    BackendIds = load_conditional_backends(Table, Severity),
    lists:filtermap(fun (BackendId) -> load_backend(Table, BackendId) end, lists:usort(BackendIds)).

%% @doc 登録済みバックエンド一覧を取得する
%%
%% TODO: returns id only
-spec which_backends(table()) -> [logi_backend:backend()].
which_backends(Table) ->
    [Backend || {{backend, _}, Backend} <- ets:tab2list(Table)].

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec add_backend(table(), logi_backend:backend()) -> ok.
add_backend(Table, Backend) ->
    true = ets:insert(Table, {{backend, logi_backend:get_id(Backend)}, Backend}),
    ok.

-spec delete_backend(table(), logi_backend:backend()) -> ok.
delete_backend(Table, Backend) ->
    true = ets:delete(Table, {backend, logi_backend:get_id(Backend)}),
    ok.

-spec add_condition(table(), logi_condition:condition(), logi_backend:backend()) -> ok.
add_condition(Table, Condition, Backend) ->
    add_condition_clauses(Table, logi_backend:get_id(Backend), logi_condition:get_normalized_spec(Condition)).

-spec add_condition_clauses(table(), logi_backend:id(), [logi:loglevel()]) -> ok.
add_condition_clauses(_Table, _BackendId, [])                         -> ok;
add_condition_clauses(Table, BackendId, [Level | Rest]) ->
    Severity = Level,
    Backends0 = load_conditional_backends(Table, Severity),
    Backends1 = lists:umerge([BackendId], Backends0),
    _ = save_conditional_backends(Table, Severity, Backends1),
    add_condition_clauses(Table, BackendId, Rest).

-spec delete_condition(table(), logi_backend:backend()) -> ok.
delete_condition(Table, Backend) ->
    DeleteId = logi_backend:get_id(Backend),
    lists:foreach(
      fun (Severity) ->
              Backends0 = load_conditional_backends(Table, Severity),
              Backends1 = lists:filter(fun (BackendId) -> BackendId =/= DeleteId end, Backends0),
              save_conditional_backends(Table, Severity, Backends1)
      end,
      logi:log_levels()).

-spec load_conditional_backends(table(), logi:severity()) -> [logi_backend:id()].
load_conditional_backends(Table, Severity) ->
    %% TODO: bagを使った方がシンプルかも(setではなく)
    try ets:lookup(Table, {severity, Severity}) of
        []              -> [];
        [{_, Backends}] -> Backends
    catch
        error:badarg -> []  % おそらく Table が存在しない
    end.

-spec save_conditional_backends(table(), logi:severity(), [logi_backend:id()]) -> ok.
save_conditional_backends(Table, Severity, Backends) ->
    true = ets:insert(Table, {{severity, Severity}, Backends}),
    ok.

-spec load_backend(table(), logi_backend:id()) -> {true, logi_backend:backend()} | false.
load_backend(Table, BackendId) ->
    case ets:lookup(Table, {backend, BackendId}) of
        []             -> false;
        [{_, Backend}] -> {true, Backend}
    end.
