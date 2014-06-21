%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログ出力条件情報を扱うためのモジュール
%% @private
-module(logi_condition).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/1,
         is_condition/1,
         get_spec/1,
         get_normalized_spec/1,
         is_satisfied/4
        ]).

-export_type([
              condition/0,
              condition_spec/0,
              condition_clause/0,
              constraint/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(CONDITION, ?MODULE).

-record(?CONDITION,
        {
          spec :: condition_spec()
        }).

-opaque condition() :: #?CONDITION{}.

-type condition_spec()   :: condition_clause() | [condition_clause()].
-type condition_clause() :: logi:log_level() | {logi:log_level(), constraint()}.
-type constraint()       :: {match, {module(), Function::atom(), Arg::term()}}
                          | none.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc ログ出力条件オブジェクトを再生する
-spec make(condition_spec()) -> condition().
make(Spec) ->
    case is_condition_spec(Spec) of
        false -> error(badarg, [Spec]);
        true  -> #?CONDITION{spec = Spec}
    end.

%% @doc 引数の値がcondition()型かどうかを判定する
-spec is_condition(condition()) -> boolean().
is_condition(X) -> is_record(X, ?CONDITION).

%% @doc 正規化された出力指定を取得する
-spec get_normalized_spec(condition()) -> [condition_clause()].
get_normalized_spec(#?CONDITION{spec = Spec}) when is_list(Spec) ->
    [case Clause of
         {_, _} -> Clause;
         Level  -> {Level, none}
     end || Clause <- Spec];
get_normalized_spec(#?CONDITION{spec = Spec}) ->
    get_normalized_spec(#?CONDITION{spec = [Spec]}).

%% @doc 出力指定を取得する
-spec get_spec(condition()) -> condition_spec().
get_spec(#?CONDITION{spec = Spec}) -> Spec.

%% @doc メタデータが指定の制約を満たしているかどうかを判定する
-spec is_satisfied(constraint(), logi_location:location(), logi:headers(), logi:metadata()) -> boolean().
is_satisfied(none, _Location, _Headers, _MetaData)              -> true;
is_satisfied({match, {M, F, Arg}}, Location, Headers, MetaData) -> M:F(Arg, Location, Headers, MetaData). % TODO: error-handling

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_condition_spec(condition_spec()) -> boolean().
is_condition_spec(Level) when is_atom(Level) -> is_log_level(Level);
is_condition_spec({Level, Constraint})       -> is_log_level(Level) andalso is_constraint(Constraint);
is_condition_spec(List) when is_list(List)   -> lists:all(fun (X) -> not is_list(X) andalso is_condition_spec(X) end, List);
is_condition_spec(_)                         -> false.

-spec is_constraint(constraint()) -> boolean().
is_constraint({match, {M, F, _}}) -> is_atom(M) andalso is_atom(F);
is_constraint(none)               -> true;
is_constraint(_)                  -> false.

-spec is_log_level(logi:log_level() | term()) -> boolean().
is_log_level(X) -> lists:member(X, logi:log_levels()).
