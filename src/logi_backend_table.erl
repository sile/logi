%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc logiのバックエンド管理用ETSに対する簡便なインタフェースを提供するモジュール
-module(logi_backend_table).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
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

%% -type entry() :: {{backend, logi:backend_id()}, #logi_backend{}}
%%                | {{severity, logi:severity()}, [logi:backend_id()], conditional_backends()}.

%% -type conditional_backends() :: [{logi:condition_clause(), logi:backend_id()}].

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc 新しいバックエンド群管理用ETSテーブルを作成する
-spec new(atom()) -> ets:tab().
new(TableName) ->
    ets:new(TableName, [set, protected, {read_concurrency, true}, named_table]).

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
    true = ets:insert(Table, {{backend, Backend#logi_backend.id}, Backend}),
    add_condition(Table, Backend#logi_backend.id, Backend#logi_backend.condition).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec add_condition(table(), logi:backend_id(), logi:condition_clause()) -> ok.
add_condition(Table, BackendId, Conditions) when is_list(Conditions) ->
    lists:foreach(fun (Condition) -> add_condition(Table, BackendId, Condition) end, Conditions);
add_condition(Table, BackendId, {Level, Condition}) ->
    lists:foreach(
      fun (UpperLevel) ->
              ConditionalBackends = find_conditions(Table, UpperLevel),
              ConditionalBackends2 = [{Condition, BackendId} | ConditionalBackends],
              store_conditions(Table, UpperLevel, ConditionalBackends2)
      end,
      logi:upper_log_levels(Level));
add_condition(Table, BackendId, Level) ->
    add_condition(Table, BackendId, {Level, always}).

-spec find_conditions(table(), logi:log_level()) -> [{logi:condition_clause(), logi:backend_id()}].
find_conditions(Table, Level) ->
    case ets:lookup(Table, {severity, Level}) of
        []                         -> [];
        [{_, ConditionalBackends}] -> ConditionalBackends
    end.

-spec store_conditions(table(), logi:log_level(), [{logi:condition_clause(), logi:backend_id()}]) -> ok.
store_conditions(Table, Level, ConditionalBackends) ->
    true = ets:insert(Table, {{severity, Level}, ConditionalBackends}),
    ok.
