%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログメッセージのヘッダ情報を管理するためのモジュール
%% @private
-module(logi_msg_header).

-include("logi_internal.hrl").
-behaviour(logi_msg_context).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([from_list/1]).

%% 'logi_msg_context' callbacks
-export([is_instance/1, empty/0]).

-export_type([header/0,
              scope/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(PROCESS_DICTIONARY_KEY, '__LOGI_MSG_HEADER__').

-define(HEADER, ?MODULE).

-record(?HEADER,
        {
          scope_to_entries = gb_trees:empty() :: gb_trees:tree(scope(), ordsets:ordset(logi:header_entry()))
        }).

-opaque header() :: #?HEADER{}.
%% 個々のイベントマネージャ用のヘッダ情報

-type scope() :: logi:header_scope() | process.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
%% @doc 空のヘッダ情報を生成する
-spec empty() -> header().
empty() ->
    #?HEADER{}.

%% @doc 連想リストからヘッダ情報を生成する
-spec from_list([{scope(), [logi:header_entry()]}]) -> header().
from_list(ScopeToEntries) when is_list(ScopeToEntries) ->
    ScopeToEntriesTree =
        lists:foldl(
          fun ({Scope, Entries}, Acc) ->
                  true = ?ASSERT_PRED_FUN(is_valid_scope, Scope),
                  true = ?ASSERT_PRED_FUN(is_valid_entries, Entries),
                  gb_trees:enter(Scope, lists:ukeysort(1, Entries), Acc)
          end,
          gb_trees:empty(),
          ScopeToEntries),
    #?HEADER{scope_to_entries = ScopeToEntriesTree}.

-spec is_instance(header() | term()) -> boolean().
is_instance(MaybeHeader) -> is_record(MaybeHeader, ?HEADER).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------------------------------------------------
-spec is_valid_scope(scope() | term()) -> boolean().
is_valid_scope(process) -> true;
is_valid_scope({_, _})  -> true;
is_valid_scope(_)       -> false.

-spec is_valid_entries([logi:header_entry()] | term()) -> boolean().
is_valid_entries([])              -> true;
is_valid_entries([{_, _} | Rest]) -> is_valid_entries(Rest);
is_valid_entries(_)               -> false.
