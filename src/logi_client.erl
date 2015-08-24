%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログ出力処理をハンドリングするクライアントモジュール
%% @private
-module(logi_client).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([ready/3, ready/4, write/4]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec ready(logi:severity(), logi_location:location(), logi:log_options()) -> Result when
      Result :: {ok, [logi_backend:backend()], logi_msg_info:info(), logi_context:context()}
              | {skip, logi_context:context()}.
ready(Severity, Location, Options) ->
    ready(maps:get(logger, Options, logi:default_logger()), Severity, Location, Options).

%% @doc ログ出力に必要な情報の準備を行う
%%
%% 引数の条件(ex. Severity)に該当するバックエンドが存在しない場合には`skip'が(第一要素として)返される
-spec ready(logi:context_ref(), logi:severity(), logi_location:location(), logi:log_options()) -> Result when
      Result :: {ok, [logi_backend:backend()], logi_msg_info:info(), logi_context:context()}
              | {skip, logi_context:context()}.
ready(ContextRef, Severity, Location, Options) when is_atom(ContextRef) ->
    Result = ready(logi:load_context(ContextRef), Severity, Location, Options),
    ok = case Result of
             {skip, Context}     -> logi:save_context(ContextRef, Context);
             {ok, _, _, Context} -> logi:save_context(ContextRef, Context)
         end,
    Result;
ready(Context0, Severity0, Location, Options) ->
    MetaData = logi_context:get_full_metadata(maps:get(metadata, Options, []), Context0),
    Headers  = logi_context:get_full_headers(maps:get(headers, Options, []), Context0),
    Logger = logi_context:get_logger(Context0),
    {Severity, Context1} = logi_context:apply_severity_mapper(Severity0, Location, Context0),
    case logi_backend_manager:select_backends(Logger, Severity, Location, Headers, MetaData) of
        []       -> {skip, Context1};
        Backends ->
            MsgInfo = logi_msg_info:make(Severity, os:timestamp(), Location, Headers, MetaData),
            case logi_context:is_output_allowed(maps:get(frequency, Options, undefined), MsgInfo, Context1) of
                {false, Context2} -> {skip, Context2};
                {true,  Context2} -> {ok, Backends, MsgInfo, Context2}
            end
    end.

%% @doc 引数のバックエンドを使ってログ出力(書き込み)処理を行う
-spec write([logi_backend:backend()], logi_msg_info:info(), io:format(), [term()]) -> ok.
write(Backends, MsgInfo, Format, Args) ->
    lists:foreach(
      fun (Backend) ->
              Module = logi_backend:get_module(Backend),
              try
                  _ = Module:write(Backend, MsgInfo, Format, Args)
              catch
                  Class:Reason ->
                      error_logger:error_report(
                        [{location, [{module, ?MODULE}, {line, ?LINE}, {pid, self()}]},
                         {mfargs, {Module, write, [Backend, MsgInfo, Format, Args]}},
                         {exception, {Class, Reason, erlang:get_stacktrace()}}])
              end
      end,
      Backends).
