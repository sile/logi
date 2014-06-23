%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログ出力処理をハンドリングするクライアントモジュール
%% @private
-module(logi_client).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         ready/4,
         write/6
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc ログ出力に必要な情報の準備を行う
%%
%% 引数の条件(ex. Severity)に該当するバックエンドが存在しない場合には`skip'が(第一要素として)返される
-spec ready(logi_context:context(), logi:severity(), logi_location:location(), logi:log_options()) -> Result when
      Result :: {ok, [logi_backend:backend()], logi_msg_info:info(), logi_context:context()}
              | {skip, logi_context:context()}.
ready(Context0, Severity, Location, Options) ->
    MetaData = logi_context:get_full_metadata(logi_util_assoc:fetch(metadata, Options, []), Context0),
    Headers  = logi_context:get_full_headers(logi_util_assoc:fetch(headers, Options, []), Context0),
    Logger = logi_context:get_logger(Context0),
    case logi_backend_manager:select_backends(Logger, Severity, Location, Headers, MetaData) of
        []       -> {skip, Context0};
        Backends ->
            case logi_context:is_output_allowed(logi_util_assoc:fetch(frequency, Options, always), Location, Context0) of
                {false, Context1}                 -> {skip, Context1};
                {{true, OmittedCount},  Context1} ->
                    MsgInfo = logi_msg_info:make(Severity, os:timestamp(), Headers, MetaData, OmittedCount),
                    {ok, Backends, MsgInfo, Context1}
            end
    end.

%% @doc 引数のバックエンドを使ってログ出力(書き込み)処理を行う
-spec write(logi_context:context(), [logi_backend:backend()], logi_location:location(), logi_msg_info:info(), io:format(), [term()]) ->
                   logi_context:context().
write(Context, Backends, Location, MsgInfo, Format, Args) ->
    ok = lists:foreach(
           fun (Backend) ->
                   Module = logi_backend:get_module(Backend),
                   try
                       _ = Module:write(Backend, Location, MsgInfo, Format, Args)
                   catch
                       Class:Reason ->
                           error_logger:error_msg(
                             binary_to_list(
                               logi_io_lib:format(
                                 "~s:write/5 failed: class=~s, reason=~4096P, trace=~4096P, backend=~4096p, location=~4096p, msg_info=~4096p, format=~p, args=~4096P",
                                 [Module, Class, Reason, 20, erlang:get_stacktrace(), 20, Backend, Location, MsgInfo, Format, Args, 20])))
                   end
           end,
           Backends),
    Context.
