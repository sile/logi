%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Logger Client
%% @private
-module(logi_client).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/2]).
-export([to_map/1]).

-export_type([client/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(CLIENT, ?MODULE).

-record(?CLIENT,
        {
          logger_id                   :: logi:logger_id(),
          headers              = none :: none | logi:headers(),
          metadata             = none :: none | logi:metadata(),
          context_handler      = none :: none | logi:context_handler(),
          frequency_controller = none :: none | logi_frequency_controller:controller()
        }).

-opaque client() :: #?CLIENT{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc TODO
-spec make(logi:logger_id(), Options) -> client() when
      Options :: #{
        headers         => logi:headers(),
        metadata        => logi:metadata(),
        context_handler => logi:context_handler()
       }.
make(LoggerId, Options) ->
    _ = is_atom(LoggerId) orelse error(badarg, [LoggerId, Options]),
    _ = is_map(Options)   orelse error(badarg, [LoggerId, Options]),
    Client = #?CLIENT{logger_id = LoggerId},
    maps:fold(
      fun (headers, Headers, Acc) ->
              _ = logi_context:is_valid_headers(Headers) orelse error(badarg, [LoggerId, Options]),
              Acc#?CLIENT{headers = Headers};
          (metadata, Metadata, Acc) ->
              _ = logi_context:is_valid_metadata(Metadata) orelse error(badarg, [LoggerId, Options]),
              Acc#?CLIENT{metadata = Metadata};
          (context_handler, ContextHandler, Acc) ->
              _ = logi_context:is_valid_context_handler(ContextHandler) orelse error(badarg, [LoggerId, Options]),
              Acc#?CLIENT{context_handler = ContextHandler}
      end,
      Client,
      maps:with([headers, metadata, context_handler], Options)).

%% @doc TODO
-spec to_map(client()) -> Map when
      Map :: #{
        logger_id            => logi:logger_id(),
        headers              => logi:headers(),
        metadata             => logi:metadata(),
        context_handler      => logi:context_handler(),
        frequency_controller => logi:frequency_controller()
       }.
to_map(Client) ->
    maps:filter(
      fun (logger_id, _) -> true;
          (_, none)      -> false;
          (_, _)         -> true
      end,
      #{
        logger_id            => Client#?CLIENT.logger_id,
        headers              => Client#?CLIENT.headers,
        metadata             => Client#?CLIENT.metadata,
        context_handler      => Client#?CLIENT.context_handler,
        frequency_controller => Client#?CLIENT.frequency_controller
       }).

%% %%------------------------------------------------------------------------------------------------------------------------
%% %% Exported API
%% %%------------------------------------------------------------------------------------------------------------------------
%% -export([ready/3, ready/4, write/4]).

%% %%------------------------------------------------------------------------------------------------------------------------
%% %% Exported Functions
%% %%------------------------------------------------------------------------------------------------------------------------
%% -spec ready(logi:severity(), logi_location:location(), logi:log_options()) -> Result when
%%       Result :: {ok, [logi_backend:backend()], logi_msg_info:info(), logi_context:context()}
%%               | {skip, logi_context:context()}.
%% ready(Severity, Location, Options) ->
%%     ready(maps:get(logger, Options, logi:default_logger()), Severity, Location, Options).

%% %% @doc ログ出力に必要な情報の準備を行う
%% %%
%% %% 引数の条件(ex. Severity)に該当するバックエンドが存在しない場合には`skip'が(第一要素として)返される
%% -spec ready(logi:context_ref(), logi:severity(), logi_location:location(), logi:log_options()) -> Result when
%%       Result :: {ok, [logi_backend:backend()], logi_msg_info:info(), logi_context:context()}
%%               | {skip, logi_context:context()}.
%% ready(ContextRef, Severity, Location, Options) when is_atom(ContextRef) ->
%%     Result = ready(logi:load_context(ContextRef), Severity, Location, Options),
%%     ok = case Result of
%%              {skip, Context}     -> logi:save_context(ContextRef, Context);
%%              {ok, _, _, Context} -> logi:save_context(ContextRef, Context)
%%          end,
%%     Result;
%% ready(Context0, Severity0, Location, Options) ->
%%     MetaData = logi_context:get_full_metadata(maps:get(metadata, Options, []), Context0),
%%     Headers  = logi_context:get_full_headers(maps:get(headers, Options, []), Context0),
%%     Logger = logi_context:get_logger(Context0),
%%     {Severity, Context1} = logi_context:apply_severity_mapper(Severity0, Location, Context0),
%%     case logi_backend_manager:select_backends(Logger, Severity, Location, Headers, MetaData) of
%%         []       -> {skip, Context1};
%%         Backends ->
%%             MsgInfo = logi_msg_info:make(Severity, os:timestamp(), Location, Headers, MetaData),
%%             case logi_context:is_output_allowed(maps:get(frequency, Options, undefined), MsgInfo, Context1) of
%%                 {false, Context2} -> {skip, Context2};
%%                 {true,  Context2} -> {ok, Backends, MsgInfo, Context2}
%%             end
%%     end.

%% %% @doc 引数のバックエンドを使ってログ出力(書き込み)処理を行う
%% -spec write([logi_backend:backend()], logi_msg_info:info(), io:format(), [term()]) -> ok.
%% write(Backends, MsgInfo, Format, Args) ->
%%     lists:foreach(
%%       fun (Backend) ->
%%               Module = logi_backend:get_module(Backend),
%%               try
%%                   _ = Module:write(Backend, MsgInfo, Format, Args)
%%               catch
%%                   Class:Reason ->
%%                       error_logger:error_report(
%%                         [{location, [{module, ?MODULE}, {line, ?LINE}, {pid, self()}]},
%%                          {mfargs, {Module, write, [Backend, MsgInfo, Format, Args]}},
%%                          {exception, {Class, Reason, erlang:get_stacktrace()}}])
%%               end
%%       end,
%%       Backends).
