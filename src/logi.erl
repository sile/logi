%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A Logger Interface Library
%%
%% TODO: 全体的に用語整理
-module(logi).

-compile({no_auto_import, [error/1, error/2]}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

%% Constants
-export([default_logger/0]).
-export([log_levels/0]).

%% Logger Process API
-export([start_logger/1, ensure_logger_started/1]).
-export([stop_logger/1]).
-export([which_loggers/0]).

%% Backend API
-export([register_backend/3]).
-export([update_backend/2]).
-export([deregister_backend/1, deregister_backend/2]).
-export([find_backend/1, find_backend/2]).
-export([which_backends/0, which_backends/1]).

%% Logging API
-export([log/2, log/3, log/4]).
-export([debug/1, debug/2, debug/3]).
-export([verbose/1, verbose/2, verbose/3]).
-export([info/1, info/2, info/3]).
-export([notice/1, notice/2, notice/3]).
-export([warning/1, warning/2, warning/3]).
-export([error/1, error/2, error/3]).
-export([critical/1, critical/2, critical/3]).
-export([alert/1, alert/2, alert/3]).
-export([emergency/1, emergency/2, emergency/3]).

-export([
         %% コンテキスト系
         make_context/0, make_context/1, make_context/2,
         save_context/1, save_context/2,
         load_context/0, load_context/1,
         which_contexts/0,

         %% ヘッダ系
         set_headers/1, set_headers/2,
         get_headers/0, get_headers/1,
         delete_headers/1, delete_headers/2,
         clear_headers/0, clear_headers/1,

         %% メタデータ系
         set_metadata/1, set_metadata/2,
         get_metadata/0, get_metadata/1,
         delete_metadata/1, delete_metadata/2,
         clear_metadata/0, clear_metadata/1

         %% TODO: location/0

        ]).

-export([on_expire/4]).  % XXX:

-export_type([
              log_level/0,
              severity/0,

              logger/0,

              metadata/0,
              metadata_entry/0,
              metadata_entry_key/0,
              metadata_entry_value/0,

              headers/0,
              header/0,
              header_key/0,
              header_value/0,

              context/0,
              context_id/0,
              context_ref/0,

              log_options/0,
              frequency_policy/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency.
-type severity()  :: log_level().

-type logger() :: atom().

-type context()     :: logi_context:context(). % opaqueにしたい
-type context_id()  :: atom().
-type context_ref() :: context() | context_id().
-type client_ref() :: context_ref().

-type headers()      :: [header()].
-type header()       :: {header_key(), header_value()}.
-type header_key()   :: term().
-type header_value() :: term().

-type metadata()             :: [metadata_entry()].
-type metadata_entry()       :: {metadata_entry_key(), metadata_entry_value()}.
-type metadata_entry_key()   :: term().
-type metadata_entry_value() :: term().

-type seconds() :: non_neg_integer().

-type frequency_policy() :: #{intensity => non_neg_integer(),
                              period    => seconds(),
                              on_expire => function(), % XXX:
                              max_flush_count => pos_integer(),
                              id => term()}. % TODO: description

-type log_options() ::
        #{
           logger => client_ref(),
           location => logi_location:location(),
           headers => headers(),  % default: []
           metadata => metadata(), % default: []
           frequency => frequency_policy()
         }. % default: always
%% TODO: location (?)

%% TODO: export
-type register_backend_options() ::
        #{
           extra_arg => logi_backend:extra_arg(),
           condition => logi_condition:spec(),
           ttl       => timeout(),
           owner     => pid(),
           if_exists => error | ignore | supersede,
           logger    => logger()
         }.

-type update_backend_options() ::
        #{
           extra_arg => logi_backend:extra_arg(),
           condition => logi_condition:spec(),
           ttl       => timeout(),
           owner     => undefined | pid(),
           logger    => logger()
         }.

-type deregister_backend_options() :: #{logger => logger()}.

-type find_backend_options() :: #{logger => logger()}.

-type which_backends_options() :: #{logger => logger()}.

-type backend_info() ::
        #{
           id        => logi_backend:id(),
           module    => logi_backend:callback_module(),
           extra_arg => logi_backend:extra_arg(),
           condition => logi_condition:spec(),
           ttl       => timeout(),
           owner     => pid()
         }.

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(DEFAULT_LOGGER, logi_default_logger).
-define(CONTEXT_TAG, '__LOGI_CONTEXT__').

%% TODO: move to logi_logger module
-define(LOGGER_RUNNING_CHECK(LoggerId),
        _ = case whereis(LoggerId) of
                undefined -> erlang:error({logger_is_not_running, LoggerId});
                _         -> ok
            end).

-define(WITH_CONTEXT(ContextRef, Fun),
        case is_atom(ContextRef) of
            false -> (Fun)(ContextRef);
            true  -> ok = save_context(ContextRef, (Fun)(load_context(ContextRef))), ContextRef
        end).

-define(WITH_READ_CONTEXT(ContextRef, Fun),
        case logi_context:is_context(ContextRef) of
            true  -> (Fun)(ContextRef);
            false -> (Fun)(load_context(ContextRef))
        end).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%% Exported Functions: Constants
%%------------------------------------------------------------------------------
%% @doc Returns the default logger
%%
%% The default logger is started automatically when logi application was started.
-spec default_logger() -> logger().
default_logger() -> ?DEFAULT_LOGGER.

%% @doc Returns the available log level list
%%
%% The log levels are ordered by the severity (The lowest severity level will appear first).
-spec log_levels() -> [log_level()].
log_levels() -> [debug, verbose, info, notice, warning, error, critical, alert, emergency].

%%------------------------------------------------------------------------------
%% Exported Functions: Logger Process API
%%------------------------------------------------------------------------------
%% @doc Starts a new logger
%%
%% If a logger with the same Id already exists, it will return `{error, already_started}'.
-spec start_logger(logger()) -> ok | {error, already_started}.
start_logger(LoggerId) when is_atom(LoggerId) ->
    case logi_backend_manager_sup:start_manager(LoggerId) of
        {ok, _Pid}                       -> ok;
        {error, {already_started, _Pid}} -> {error, already_started};
        Other                            -> erlang:error({unexpected_result, Other}, [LoggerId])
    end;
start_logger(LoggerId) -> erlang:error(badarg, [LoggerId]).

%% @doc Equivalent to {@link start_logger/1} except it returns `ok' for already started loggers
-spec ensure_logger_started(logger()) -> ok.
ensure_logger_started(LoggerId) ->
    case start_logger(LoggerId) of
        ok                       -> ok;
        {error, already_started} -> ok
    end.

%% @doc Stops the logger
%%
%% If the logger `LoggerId' have not been started, it will be silently ignored.
-spec stop_logger(logger()) -> ok.
stop_logger(LoggerId) when is_atom(LoggerId) -> logi_backend_manager_sup:stop_manager(LoggerId);
stop_logger(LoggerId)                        -> erlang:error(badarg, [LoggerId]).

%% @doc Returns a list of all running loggers
-spec which_loggers() -> [logger()].
which_loggers() -> logi_backend_manager_sup:which_managers().

%%------------------------------------------------------------------------------
%% Exported Functions: Backend API
%%------------------------------------------------------------------------------
%% @doc Registers the backend
-spec register_backend(logi_backend:id(), logi_backend:callback_module(), register_backend_options()) ->
                              {ok, OldBackend} | {error, Reason} when
      OldBackend :: undefined | backend_info(),
      Reason     :: {already_registered, backend_info()}.
register_backend(BackendId, BackendModlue, Options) ->
    logi_backend_manager:register_backend(BackendId, BackendModlue, Options).

%% @doc Updates the backend
-spec update_backend(logi_backend:id(), update_backend_options()) -> {ok, OldBackend} | {error, Reason} when
      OldBackend :: undefined | backend_info(),
      Reason     :: unregistered.
update_backend(BackendId, Options) ->
    logi_backend_manager:update_backend(BackendId, Options).

%% @equiv deregister_backend(BackendId, #{})
-spec deregister_backend(logi_backend:id()) -> DeregisteredBackend when
      DeregisteredBackend :: undefined | backend_info().
deregister_backend(BackendId) ->
    deregister_backend(BackendId, #{}).

%% @doc Deregisters the backend
-spec deregister_backend(logi_backend:id(), deregister_backend_options()) -> DeregisteredBackend when
      DeregisteredBackend :: undefined | backend_info().
deregister_backend(BackendId, Options) ->
    logi_backend_manager:deregister_backend(BackendId, Options).

%% @equiv find_backend(BackendId, #{})
-spec find_backend(logi_backend:id()) -> {ok, backend_info()} | {error, not_found}.
find_backend(BackendId) ->
    find_backend(BackendId, #{}).

%% @doc Finds the backend
-spec find_backend(logi_backend:id(), find_backend_options()) -> {ok, backend_info()} | {error, not_found}.
find_backend(BackendId, Options) ->
    logi_backend_manager:find_backend(BackendId, Options).

%% @equiv which_backends(#{})
-spec which_backends() -> [logi_backend:id()].
which_backends() ->
    which_backends(#{}).

%% @doc Returns a list of registered backends
-spec which_backends(which_backends_options()) -> [logi_backend:id()].
which_backends(Options) ->
    logi_backend_manager:which_backends(Options).

%%------------------------------------------------------------------------------
%% Exported Functions: Logging API
%%------------------------------------------------------------------------------
%% @equiv log(Severity, Format, [])
-spec log(severity(), io:format()) -> client_ref().
log(Severity, Format) -> log(Severity, Format, []).

%% @equiv log(Severity, Format, Args, #{})
-spec log(severity(), io:format(), [term()]) -> client_ref().
log(Severity, Format, Args) -> log(Severity, Format, Args, #{}).

%% @doc Outputs the log message
%%
%% TODO: doc: logi_transform
-spec log(severity(), io:format(), [term()], log_options()) -> client_ref().
log(Severity, Format, Args, Options) ->
    ClientRef = maps:get(logger, Options, default_logger()),
    ?WITH_CONTEXT(ClientRef,
                  fun (Client0) ->
                          case logi_client:ready(Client0, Severity, Options) of
                              {skip, Client1}                  -> Client1;
                              {ok, Backends, MsgInfo, Client1} -> _ = logi_client:write(Backends, MsgInfo, Format, Args), Client1
                          end
                  end).

-spec debug(io:format()) -> context_ref().
debug(Format) -> debug(Format, []).

-spec debug(io:format(), [term()]) -> context_ref().
debug(Format, Args) -> debug(Format, Args, #{}).

-spec debug(io:format(), [term()], log_options()) -> context_ref().
debug(Format, Args, Options) -> log(debug, Format, Args, Options).

-spec verbose(io:format()) -> context_ref().
verbose(Format) -> verbose(Format, []).

-spec verbose(io:format(), [term()]) -> context_ref().
verbose(Format, Args) -> verbose(Format, Args, #{}).

-spec verbose(io:format(), [term()], log_options()) -> context_ref().
verbose(Format, Args, Options) -> log(verbose, Format, Args, Options).

-spec info(io:format()) -> context_ref().
info(Format) -> info(Format, []).

-spec info(io:format(), [term()]) -> context_ref().
info(Format, Args) -> info(Format, Args, #{}).

-spec info(io:format(), [term()], log_options()) -> context_ref().
info(Format, Args, Options) -> log(info, Format, Args, Options).

-spec notice(io:format()) -> context_ref().
notice(Format) -> notice(Format, []).

-spec notice(io:format(), [term()]) -> context_ref().
notice(Format, Args) -> notice(Format, Args, #{}).

-spec notice(io:format(), [term()], log_options()) -> context_ref().
notice(Format, Args, Options) -> log(notice, Format, Args, Options).

-spec warning(io:format()) -> context_ref().
warning(Format) -> warning(Format, []).

-spec warning(io:format(), [term()]) -> context_ref().
warning(Format, Args) -> warning(Format, Args, #{}).

-spec warning(io:format(), [term()], log_options()) -> context_ref().
warning(Format, Args, Options) -> log(warning, Format, Args, Options).

-spec error(io:format()) -> context_ref().
error(Format) -> error(Format, []).

-spec error(io:format(), [term()]) -> context_ref().
error(Format, Args) -> error(Format, Args, #{}).

-spec error(io:format(), [term()], log_options()) -> context_ref().
error(Format, Args, Options) -> log(error, Format, Args, Options).

-spec critical(io:format()) -> context_ref().
critical(Format) -> critical(Format, []).

-spec critical(io:format(), [term()]) -> context_ref().
critical(Format, Args) -> critical(Format, Args, #{}).

-spec critical(io:format(), [term()], log_options()) -> context_ref().
critical(Format, Args, Options) -> log(critical, Format, Args, Options).

-spec alert(io:format()) -> context_ref().
alert(Format) -> alert(Format, []).

-spec alert(io:format(), [term()]) -> context_ref().
alert(Format, Args) -> alert(Format, Args, #{}).

-spec alert(io:format(), [term()], log_options()) -> context_ref().
alert(Format, Args, Options) -> log(alert, Format, Args, Options).

%% TODO: obsolute annotation (?)
-spec emergency(io:format()) -> context_ref().
emergency(Format) -> emergency(Format, []).

-spec emergency(io:format(), [term()]) -> context_ref().
emergency(Format, Args) -> emergency(Format, Args, #{}).

-spec emergency(io:format(), [term()], log_options()) -> context_ref().
emergency(Format, Args, Options) -> log(emergency, Format, Args, Options).

%%------------------------------------------------------------------------------
%% Exported Functions: Context
%%------------------------------------------------------------------------------
%% @equiv make_context(default_logger())
-spec make_context() -> context().
make_context() -> make_context(?DEFAULT_LOGGER).

%% @equiv make_context(LoggerId, [])
-spec make_context(logger()) -> context().
make_context(LoggerId) -> logi_context:make(LoggerId).

%% @doc ログ出力コンテキストを生成する
-spec make_context(logger(), Options) -> context() when
      Options :: [Option],
      Option  :: {headers, headers()} | {metadata, metadata()}.
make_context(LoggerId, Options) ->
    logi_context:make(LoggerId,
                      logi_util_assoc:fetch(headers, Options, []),
                      logi_util_assoc:fetch(metadata, Options, [])).

%% @equiv save_context(default_logger(), Context)
-spec save_context(context()) -> ok.
save_context(Context) ->
    save_context(?DEFAULT_LOGGER, Context).

%% @doc ログ出力コンテキストをプロセス辞書に保存する
-spec save_context(context_id(), context()) -> ok.
save_context(ContextId, Context) ->
    case is_atom(ContextId) andalso logi_context:is_context(Context) of
        false -> erlang:error(badarg, [ContextId, Context]);
        true  -> _ = put({?CONTEXT_TAG, ContextId}, Context), ok
    end.

%% @equiv load_context(default_logger())
-spec load_context() -> context().
load_context() ->
    load_context(?DEFAULT_LOGGER).

%% @doc ログ出力コンテキストをプロセス辞書からロードする
%%
%% 指定されたIDのコンテキストが存在しない場合は`make_context(ContextId)'が代わりのデフォルト値として使用される。<br />
-spec load_context(context_id()) -> context().
load_context(ContextId) when is_atom(ContextId) ->
    case get({?CONTEXT_TAG, ContextId}) of
        undefined -> make_context(ContextId);
        Context   -> Context
    end;
load_context(ContextId) -> erlang:error(badarg, [ContextId]).

%% @doc 保存されているコンテキスト一覧を取得する
-spec which_contexts() -> [{context_id(), context()}].
which_contexts() ->
    [{Id, Context} || {{?CONTEXT_TAG, Id}, Context} <- get()].

%%------------------------------------------------------------------------------
%% Exported Functions: Headers
%%------------------------------------------------------------------------------
%% @equiv set_headers(default_logger(), Headers)
-spec set_headers(headers()) -> context_id().
set_headers(Headers) -> set_headers(?DEFAULT_LOGGER, Headers).

%% @doc ヘッダ群を設定する
%%
%% キー名が同じヘッダが存在する場合は値が上書きされる. <br />
%% それ以外の既存のヘッダは、そのまま保持され続ける.
-spec set_headers(context_ref(), headers()) -> context_ref().
set_headers(ContextRef, Headers) ->
    ok = logi_util_assoc:assert_assoc_list(Headers),
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          Merged = lists:ukeymerge(1, lists:ukeysort(1, Headers), get_headers(Context)),
                          logi_context:set_headers(Merged, Context)
                  end).

%% @equiv get_headers(default_logger())
-spec get_headers() -> headers().
get_headers() -> get_headers(?DEFAULT_LOGGER).

%% @doc ヘッダ群を取得する
-spec get_headers(context_ref()) -> headers().
get_headers(ContextRef) ->
    ?WITH_READ_CONTEXT(ContextRef, fun logi_context:get_headers/1).

%% @equiv default_logger(default_logger(), Keys)
-spec delete_headers([header_key()]) -> context_id().
delete_headers(Keys) -> delete_headers(?DEFAULT_LOGGER, Keys).

%% @doc 指定されたキーを保持するヘッダ群の削除する
-spec delete_headers(context_ref(), [header_key()]) -> context_ref().
delete_headers(ContextRef, Keys) ->
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          Headers0 = get_headers(Context),
                          Headers1 = lists:foldl(fun (Key, Acc) -> lists:keydelete(Key, 1, Acc) end, Headers0, Keys),
                          logi_context:set_headers(Headers1, Context)
                  end).

%% @equiv clear_headers(default_logger())
clear_headers() -> clear_headers(?DEFAULT_LOGGER).

%% @doc ヘッダ群をクリアする
-spec clear_headers(context_ref()) -> context_ref().
clear_headers(ContextRef) ->
    ?WITH_CONTEXT(ContextRef, fun (Context) -> logi_context:set_headers([], Context) end).

%%------------------------------------------------------------------------------
%% Exported Functions: MetaData
%%------------------------------------------------------------------------------
%% @equiv set_metadata(default_logger(), MetaData)
-spec set_metadata(metadata()) -> context_id().
set_metadata(MetaData) -> set_metadata(?DEFAULT_LOGGER, MetaData).

%% @doc ヘッダ群を設定する
%%
%% キー名が同じエントリが存在する場合は値が上書きされる. <br />
%% それ以外の既存のエントリは、そのまま保持され続ける.
-spec set_metadata(context_ref(), metadata()) -> context_ref().
set_metadata(ContextRef, MetaData) ->
    ok = logi_util_assoc:assert_assoc_list(MetaData),
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          Merged = lists:ukeymerge(1, lists:ukeysort(1, MetaData), get_metadata(Context)),
                          logi_context:set_metadata(Merged, Context)
                  end).

%% @equiv get_metadata(default_logger())
-spec get_metadata() -> metadata().
get_metadata() -> get_metadata(?DEFAULT_LOGGER).

%% @doc メタデータを取得する
-spec get_metadata(context_ref()) -> metadata().
get_metadata(ContextRef) ->
    ?WITH_READ_CONTEXT(ContextRef, fun logi_context:get_metadata/1).

%% @equiv delete_metadata(default_logger(), Keys)
-spec delete_metadata([header_key()]) -> context_id().
delete_metadata(Keys) ->
    delete_metadata(?DEFAULT_LOGGER, Keys).

%% @doc メタデータから指定されたキーのエントリ群の削除する
-spec delete_metadata(context_ref(), [header_key()]) -> context_ref().
delete_metadata(ContextRef, Keys) ->
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          MetaData0 = get_metadata(Context),
                          MetaData1 = lists:foldl(fun (Key, Acc) -> lists:keydelete(Key, 1, Acc) end, MetaData0, Keys),
                          logi_context:set_metadata(MetaData1, Context)
                  end).

%% @equiv clear_metadata(default_logger())
clear_metadata() -> clear_metadata(?DEFAULT_LOGGER).

%% @doc メタデータをクリアする
-spec clear_metadata(context_ref()) -> context_ref().
clear_metadata(ContextRef) ->
    ?WITH_CONTEXT(ContextRef, fun (Context) -> logi_context:set_metadata([], Context) end).


%% TODO
on_expire(Context, Id, Count, Info) ->
    %% TODO: max_flush_count=0
    Duration = timer:now_diff(os:timestamp(), logi_msg_info:get_timestamp(Info)) / 1000 / 1000,
    log(logi_msg_info:get_severity(Info),
        "Over ~p seconds, ~p messages were dropped (id: ~p)", [Duration, Count, Id],
        #{logger => Context,
          location => logi_msg_info:get_location(Info),
          headers => logi_msg_info:get_headers(Info),
          metadata => logi_msg_info:get_metadata(Info)}).
