%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログの出力頻度調整を担当するモジュール
%% @private
-module(logi_frequency_control).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         is_logging_turn/3
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(COUNT_KEY(Id), {'LOGI_FREQUENCY_CONTROL_COUNT', Id}).
-define(TIME_KEY(Id), {'LOGI_FREQUENCY_CONTROL_TIME', Id}).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_logging_turn(module(), pos_integer(), #logi_log_option{}) -> false | {true, non_neg_integer()}.
is_logging_turn(Module, Line, Options) ->
    is_logging_turn(Options#logi_log_option.frequency, {Module, Line}).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_logging_turn(logi:frequency_policy(), term()) -> false | {true, non_neg_integer()}.
is_logging_turn(always, _Id) ->
    {true, 0};
is_logging_turn(once, Id) ->
    case load_count(Id) of
        0 -> {true, 0};
        _ -> false
    end;
is_logging_turn({interval_count, N}, Id) ->
    Count = load_count(Id),
    case Count rem N of
        0 ->
            ok = set_count(Id, 1),
            {true, max(0, Count - 1)};
        _ ->
            ok = set_count(Id, Count + 1),
            false
    end;
is_logging_turn({interval_time, T}, Id) ->
    Previous = load_last_time(Id),
    Now = os:timestamp(),
    case timer:now_diff(Now, Previous) div 1000 < T of
        true ->
            ok = set_count(Id, load_count(Id) + 1),
            false;
        false ->
            Count = load_count(Id),
            ok = set_last_time(Id, Now),
            ok = set_count(Id, 0),
            {true, Count}
    end.

-spec load_count(term()) -> non_neg_integer().
load_count(Id) ->
    case get(?COUNT_KEY(Id)) of
        undefined -> 0;
        Count     -> Count
    end.

-spec set_count(term(), non_neg_integer()) -> ok.
set_count(Id, Count) ->
    _ = put(?COUNT_KEY(Id), Count),
    ok.

-spec load_last_time(term()) -> erlang:timestamp().
load_last_time(Id) ->
    case get(?TIME_KEY(Id)) of
        undefined -> {0, 0, 0};
        Timestamp -> Timestamp
    end.

-spec set_last_time(term(), erlang:timestamp()) -> ok.
set_last_time(Id, Timestamp) ->
    _ = put(?TIME_KEY(Id), Timestamp),
    ok.
