%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_util_assoc).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         fetch/3,
         is_assoc_list/1,
         assert_assoc_list/1
        ]).

-export_type([
              assoc_list/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type assoc_list() :: [{Key::term(), Value::term()}].

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-spec fetch(term(), assoc_list(), term()) -> term().
fetch(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        _          -> Default
    end.

%% @doc 引数の値が連想リスト形式かどうかを判定する
-spec is_assoc_list(assoc_list() | term()) -> boolean().
is_assoc_list(List) when is_list(List) -> lists:all(fun ({_, _}) -> true; (_) -> false end, List);
is_assoc_list(_)                       -> false.

-spec assert_assoc_list(assoc_list() | term()) -> ok.
assert_assoc_list(List) ->
    case is_assoc_list(List) of
        false -> error(badarg, [List]);
        true  -> ok
    end.
