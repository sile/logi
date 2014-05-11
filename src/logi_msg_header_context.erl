%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc プロセスに紐付くログメッセージのヘッダ情報群を管理するためのモジュール
%% @private
-module(logi_msg_header_context).

-include("logi_internal.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([load_context/0,
         save_context/1,
         erase_context/0]).

-export([get_header/2,
         update_header/3,
         delete_header/2]).

-export_type([context/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(PROCESS_DICTIONARY_KEY, '__LOGI_MSG_HEADER_CONTEXT__').

-opaque context() :: [{logi:event_manager_ref(), logi_msg_header:header()}].

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
-spec save_context(context()) -> ok.
save_context(Context) ->
    ok = validate_context(Context),
    _ = put(?PROCESS_DICTIONARY_KEY, Context),
    ok.

%% @doc ヘッダ情報のコンテキストをプロセス辞書から削除する
-spec erase_context() -> ok.
erase_context() ->
    _ = erase(?PROCESS_DICTIONARY_KEY),
    ok.

%% @doc イベントマネージャに紐付くヘッダ情報を取得する
-spec get_header(logi:event_manager_ref(), context()) -> logi_msg_header:header().
get_header(ManagerRef, Context) ->
    case lists:keyfind(ManagerRef, 1, Context) of
        false       -> logi_msg_header:empty();
        {_, Header} -> Header
    end.

%% @doc イベントマネージャに紐付くヘッダ情報を更新する
-spec update_header(logi:event_manager_ref(), UpdateFun, context()) -> context() when
      UpdateFun :: fun ((logi_msg_header:header()) -> logi_msg_header:header()).
update_header(ManagerRef, UpdateFun, Context0) ->
    {Header1, Context2} =
        case lists:keytake(ManagerRef, 1, Context0) of
            false                           -> {logi_msg_header:empty(), Context0};
            {value, {_, Header0}, Context1} -> {Header0, Context1}
        end,
    Header2 = UpdateFun(Header1),
    [{ManagerRef, Header2} | Context2].

%% @doc イベントマネージャに紐付くヘッダ情報を削除する
-spec delete_header(logi:event_manager_ref(), context()) -> context().
delete_header(ManagerRef, Context) ->
    lists:keydelete(ManagerRef, 1, Context).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------------------------------------------------
-spec validate_context(context() | term()) -> ok.
validate_context([]) ->
    ok;
validate_context([{ManagerRef, Header} | Rest]) ->
    true = ?ASSERT_PRED_FUN(logi_event_manager, is_event_manager_ref, ManagerRef),
    true = ?ASSERT_PRED_FUN(logi_msg_header, is_header, Header),
    validate_context(Rest);
validate_context([WrongEntry | _]) ->
    error({wrong_header_context_entry, WrongEntry});
validate_context(WrongContext) ->
    error({wrong_header_context, WrongContext}).
