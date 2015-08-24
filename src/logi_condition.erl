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
         get_normalized_spec/1
        ]).

-export_type([
              condition/0,
              spec/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(CONDITION, ?MODULE).

-record(?CONDITION,
        {
          spec :: spec()
        }).

-opaque condition() :: #?CONDITION{}.

-type spec() :: logi:log_level() | {logi:log_level(), logi:log_level()} | [logi:log_level()]. % XXX:

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc ログ出力条件オブジェクトを再生する
-spec make(spec()) -> condition().
make(Spec) ->
    case is_condition_spec(Spec) of
        false -> error(badarg, [Spec]);
        true  -> #?CONDITION{spec = Spec}
    end.

%% @doc 引数の値がcondition()型かどうかを判定する
-spec is_condition(condition()) -> boolean().
is_condition(X) -> is_record(X, ?CONDITION).

%% @doc 出力指定を取得する
-spec get_spec(condition()) -> spec().
get_spec(#?CONDITION{spec = Spec}) -> Spec.

%% TODO:
-spec get_normalized_spec(condition()) -> [logi:loglevel()].
get_normalized_spec(#?CONDITION{spec = Spec}) ->
    case Spec of
        {Min, Max} ->
            lists:reverse(
              lists:dropwhile(fun (L) -> Max =/= L end,
                              lists:reverse(lists:dropwhile(fun (L) -> Min =/= L end, logi:log_levels()))));
        Level when is_atom(Level) ->
            lists:dropwhile(fun (L) -> Level =/= L end, logi:log_levels());
        Levels ->
            lists:filter(fun (L) -> lists:member(L, Levels) end, logi:log_levels())
    end.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_condition_spec(spec()) -> boolean().
is_condition_spec(Level) when is_atom(Level) -> is_log_level(Level);
is_condition_spec({MinLevel, MaxLevel})      -> is_log_level(MinLevel) andalso is_log_level(MaxLevel);
is_condition_spec(List) when is_list(List)   -> lists:all(fun is_log_level/1, List);
is_condition_spec(_)                         -> false.

-spec is_log_level(logi:log_level() | term()) -> boolean().
is_log_level(X) -> lists:member(X, logi:log_levels()).
