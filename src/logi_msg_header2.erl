%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログメッセージのヘッダ情報を管理するためのモジュール
%% @private
-module(logi_msg_header2).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([load_context/0,
         save_context/1,
         erase_context/0]).

-export_type([context/0,
              header/0]).

%%          set_entries/3,
%%          unset_entries/2, unset_entries/3,
%%          get_entries/2
%%         ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(PROCESS_DICTIONARY_KEY, '__LOGI_MSG_HEADER__').

-define(HEADER, ?MODULE).

-record(?HEADER,
        {
          scope_to_entries = gb_trees:empty() :: gb_trees:tree(scope(), [logi:header_entry()])
        }).

-opaque context() :: [{logi:event_manager_ref(), header()}].
%% 各プロセスに紐付く全てのヘッダ情報を保持するコンテキストデータ

-opaque header() :: #?HEADER{}.
%% 個々のイベントマネージャ用のヘッダ情報

-type scope() :: logi:header_scope() | process.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
%% @doc ヘッダ情報のコンテキストをプロセス辞書からロードする
-spec load_context() -> context().
load_context() ->
    case get(?PROCESS_DICTIONARY_KEY) of
        undefined -> [];
        Context   -> Context
    end.

%% @doc ヘッダ情報のコンテキストをプロセス辞書に保存する
%%
%% 渡された引数の形式が不正な場合は、エラーとはならずに単に無視される(古いコンテキストがそのまま有効であり続ける)
-spec save_context(context()) -> ok.
save_context(Context) ->
    case validate_context(Context) of
        false -> ok;
        true  ->
            _ = put(?PROCESS_DICTIONARY_KEY, Context),
            ok
    end.

%% @doc ヘッダ情報のコンテキストをプロセス辞書から削除する
-spec erase_context() -> ok.
erase_context() ->
    _ = erase(?PROCESS_DICTIONARY_KEY),
    ok.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------------------------------------------------
-spec validate_context(context() | term()) -> boolean().
validate_context([{ManagerRef, Header} | Rest]) ->
    case logi_event_manager:is_event_manager_ref(ManagerRef) andalso is_record(?HEADER, Header) of
        false ->
            false;
        true ->
            validate_context(Rest)
    end.
