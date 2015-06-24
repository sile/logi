%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログ出力用の各種機能を提供するモジュール
-module(logi).

-compile({no_auto_import, [error/1, error/2]}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         %% 定数系
         default_logger/0,
         log_levels/0,

         %% ロガープロセス系
         start_logger/1, ensure_logger_started/1,
         stop_logger/1,
         which_loggers/0,

         %% バックエンド系
         set_backend/2, set_backend/3,
         delete_backend/1, delete_backend/2,
         find_backend/1, find_backend/2,
         which_backends/0, which_backends/1,
         get_condition/1, get_condition/2,
         set_condition/2, set_condition/3,

         %% ログ出力
         log/5, log/6,

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
         clear_metadata/0, clear_metadata/1,

         %% TODO: location/0

         %% ログ出力用の代替関数 (通常は`{parse_transform, logi_transform}'を指定してコンパイルすることを推奨)
         debug/1, debug/2, debug/3, debug_opt/2, debug_opt/3, debug_opt/4,
         verbose/1, verbose/2, verbose/3, verbose_opt/2, verbose_opt/3, verbose_opt/4,
         info/1, info/2, info/3, info_opt/2, info_opt/3, info_opt/4,
         notice/1, notice/2, notice/3, notice_opt/2, notice_opt/3, notice_opt/4,
         warning/1, warning/2, warning/3, warning_opt/2, warning_opt/3, warning_opt/4,
         error/1, error/2, error/3, error_opt/2, error_opt/3, error_opt/4,
         critical/1, critical/2, critical/3, critical_opt/2, critical_opt/3, critical_opt/4,
         alert/1, alert/2, alert/3, alert_opt/2, alert_opt/3, alert_opt/4,
         emergency/1, emergency/2, emergency/3, emergency_opt/2, emergency_opt/3, emergency_opt/4
        ]).

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
              log_option/0,
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

-type headers()      :: [header()].
-type header()       :: {header_key(), header_value()}.
-type header_key()   :: term().
-type header_value() :: term().

-type metadata()             :: [metadata_entry()].
-type metadata_entry()       :: {metadata_entry_key(), metadata_entry_value()}.
-type metadata_entry_key()   :: term().
-type metadata_entry_value() :: term().

-type frequency_policy() :: always
                          | once
                          | {once_in_times, Times::pos_integer()}
                          | {interval, MilliSeconds::non_neg_integer()}.

-type log_options() :: [log_option()].
-type log_option() :: {headers, headers()}             % default: []
                    | {metadata, metadata()}           % default: []
                    | {frequency, frequency_policy()}. % default: always

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(DEFAULT_LOGGER, logi_default_logger).
-define(CONTEXT_TAG, '__LOGI_CONTEXT__').

-define(LOGGER_RUNNING_CHECK(LoggerId),
        _ = case whereis(LoggerId) of
                undefined -> erlang:error({logger_not_running, LoggerId});
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
%% Exported Functions: Constant
%%------------------------------------------------------------------------------
%% @doc デフォルトのロガーを返す
%%
%% このロガーはlogiアプリケーションの開始に合わせて自動的に起動される
-spec default_logger() -> logger().
default_logger() -> ?DEFAULT_LOGGER.

%% @doc 利用可能なログレベル一覧を返す
%%
%% 結果の並び順はログレベルの昇順
-spec log_levels() -> [log_level()].
log_levels() -> [debug, verbose, info, notice, warning, error, critical, alert, emergency].

%%------------------------------------------------------------------------------
%% Exported Functions: Logger Process
%%------------------------------------------------------------------------------
%% @doc ロガーを起動する
%%
%% 既に同名のロガーが起動している場合は`{error, already_started}'が返される
-spec start_logger(logger()) -> ok | {error, Reason} when
      Reason :: already_started | term().
start_logger(LoggerId) when is_atom(LoggerId) ->
    case logi_backend_manager_sup:start_manager(LoggerId) of
        {ok, _Pid}                       -> ok;
        {error, {already_started, _Pid}} -> {error, already_started};
        Other                            -> Other
    end;
start_logger(LoggerId) -> erlang:error(badarg, [LoggerId]).

%% @doc まだ未起動の場合は、指定のロガーを起動する
-spec ensure_logger_started(logger()) -> ok | {error, Reason::term()}.
ensure_logger_started(LoggerId) ->
    case start_logger(LoggerId) of
        {error, already_started} -> ok;
        Other                    -> Other
    end.

%% @doc ロガーを停止する
%%
%% 指定のロガーが存在しない場合でもエラーとはならずに単に無視される
-spec stop_logger(logger()) -> ok.
stop_logger(LoggerId) when is_atom(LoggerId) -> logi_backend_manager_sup:stop_manager(LoggerId);
stop_logger(LoggerId)                        -> erlang:error(badarg, [LoggerId]).

%% @doc 起動中のロガー一覧を取得する
-spec which_loggers() -> [logger()].
which_loggers() -> logi_backend_manager_sup:which_managers().

%%------------------------------------------------------------------------------
%% Exported Functions: Backend
%%------------------------------------------------------------------------------
%% @equiv set_backend(default_logger(), BackendSpec, ConditionSpec)
-spec set_backend(logi_backend:spec(), logi_condition:spec()) -> ok.
set_backend(BackendSpec, ConditionSpec) ->
    set_backend(?DEFAULT_LOGGER, BackendSpec, ConditionSpec).

%% @doc バックエンドを登録する
%%
%% 既に同じIDのバックエンドが登録済みの場合は、内容が更新される
-spec set_backend(logger(), logi_backend:spec(), logi_condition:spec()) -> ok.
set_backend(LoggerId, BackendSpec, ConditionSpec) ->
    ?LOGGER_RUNNING_CHECK(LoggerId),
    Condition = logi_condition:make(ConditionSpec),
    Backend = logi_backend:make(BackendSpec),
    logi_backend_manager:set_backend(LoggerId, Backend, Condition).

%% @equiv delete_backend(default_logger(), BackendId)
-spec delete_backend(logi_backend:id()) -> ok.
delete_backend(BackendId) ->
    delete_backend(?DEFAULT_LOGGER, BackendId).

%% @doc バックエンドを削除する
%%
%% 存在しないバックエンドが指定された場合でもエラーとはならずに単に無視される
-spec delete_backend(logger(), logi_backend:id()) -> ok.
delete_backend(LoggerId, BackendId) ->
    ?LOGGER_RUNNING_CHECK(LoggerId),
    logi_backend_manager:delete_backend(LoggerId, BackendId).

%% @equiv find_backend(default_logger(), BackendId)
-spec find_backend(logi_backend:id()) -> {ok, logi_backend:backend()} | {error, not_found}.
find_backend(BackendId) ->
    find_backend(?DEFAULT_LOGGER, BackendId).

%% @doc バックエンドを検索する
-spec find_backend(logger(), logi_backend:id()) -> {ok, logi_backend:backend()} | {error, not_found}.
find_backend(LoggerId, BackendId) ->
    ?LOGGER_RUNNING_CHECK(LoggerId),
    logi_backend_manager:find_backend(LoggerId, BackendId).

%% @equiv which_backends(default_logger())
-spec which_backends() -> [logi_backend:backend()].
which_backends() ->
    which_backends(?DEFAULT_LOGGER).

%% @doc 登録されているバックエンド一覧を取得する
-spec which_backends(logger()) -> [logi_backend:backend()].
which_backends(LoggerId) ->
    ?LOGGER_RUNNING_CHECK(LoggerId),
    logi_backend_manager:which_backends(LoggerId).

%% @equiv get_condition(default_logger(), BackendId)
-spec get_condition(logi_backend:id()) -> {ok, logi_condition:condition()} | {error, not_found}.
get_condition(BackendId) ->
    get_condition(?DEFAULT_LOGGER, BackendId).

%% @doc バックエンドのログ出力条件を取得する
-spec get_condition(logger(), logi_backend:id()) -> {ok, logi_condition:condition()} | {error, not_found}.
get_condition(LoggerId, BackendId) ->
    ?LOGGER_RUNNING_CHECK(LoggerId),
    case logi_backend_manager:get_condition(LoggerId, BackendId) of
        {ok, Condition} -> {ok, logi_condition:get_spec(Condition)};
        Other           -> Other
    end.

%% @equiv set_condition(default_logger(), BackendId, ConditionSpec)
-spec set_condition(logi_backend:id(), logi_condition:spec()) -> ok | {error, not_found}.
set_condition(BackendId, ConditionSpec) ->
    set_condition(?DEFAULT_LOGGER, BackendId, ConditionSpec).

%% @doc バックエンドのログ出力条件を設定する
-spec set_condition(logger(), logi_backend:id(), logi_condition:spec()) -> ok | {error, not_found}.
set_condition(LoggerId, BackendId, ConditionSpec) ->
    ?LOGGER_RUNNING_CHECK(LoggerId),
    Condition = logi_condition:make(ConditionSpec),
    logi_backend_manager:set_condition(LoggerId, BackendId, Condition).

%%------------------------------------------------------------------------------
%% Exported Functions: Log
%%------------------------------------------------------------------------------
%% @equiv log(default_logger(), Severity, Location, Format, Args, Options)
-spec log(severity(), logi_location:location(), io:format(), [term()], log_options()) -> context_ref().
log(Severity, Location, Format, Args, Options) ->
    log(?DEFAULT_LOGGER, Severity, Location, Format, Args, Options).

%% @doc ログを出力する
%%
%% 通常は`logi_transform'モジュールが適用する関数マクロ群を通してログ出力を行い、この関数は直接は使用されない
-spec log(context_ref(), severity(), logi_location:location(), io:format(), [term()], log_options()) -> context_ref().
log(ContextRef, Severity, Location, Format, Args, Options) ->
    ?WITH_CONTEXT(ContextRef,
                  fun (Context0) ->
                          case logi_client:ready(Context0, Severity, Location, Options) of
                              {skip, Context1}                  -> Context1;
                              {ok, Backends, MsgInfo, Context1} ->
                                  logi_client:write(Context1, Backends, Location, MsgInfo, Format, Args)
                          end
                  end).

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
delete_headers(Keys) -> delete_backend(?DEFAULT_LOGGER, Keys).

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


%%------------------------------------------------------------------------------
%% Exported Functions: Log Output
%%------------------------------------------------------------------------------
-spec debug(io:format()) -> context_ref().
debug(Format) -> debug(Format, []).

-spec debug(io:format(), [term()]) -> context_ref().
debug(Format, Args) -> debug(?DEFAULT_LOGGER, Format, Args).

-spec debug(context_ref(), io:format(), [term()]) -> context_ref().
debug(ContextRef, Format, Args) -> debug_opt(ContextRef, Format, Args, []).

-spec debug_opt(io:format(), log_options()) -> context_ref().
debug_opt(Format, Options) -> debug_opt(Format, [], Options).

-spec debug_opt(io:format(), [term()], log_options()) -> context_ref().
debug_opt(Format, Args, Options) -> debug_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec debug_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
debug_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, debug, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec verbose(io:format()) -> context_ref().
verbose(Format) -> verbose(Format, []).

-spec verbose(io:format(), [term()]) -> context_ref().
verbose(Format, Args) -> verbose(?DEFAULT_LOGGER, Format, Args).

-spec verbose(context_ref(), io:format(), [term()]) -> context_ref().
verbose(ContextRef, Format, Args) -> verbose_opt(ContextRef, Format, Args, []).

-spec verbose_opt(io:format(), log_options()) -> context_ref().
verbose_opt(Format, Options) -> verbose_opt(Format, [], Options).

-spec verbose_opt(io:format(), [term()], log_options()) -> context_ref().
verbose_opt(Format, Args, Options) -> verbose_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec verbose_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
verbose_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, verbose, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec info(io:format()) -> context_ref().
info(Format) -> info(Format, []).

-spec info(io:format(), [term()]) -> context_ref().
info(Format, Args) -> info(?DEFAULT_LOGGER, Format, Args).

-spec info(context_ref(), io:format(), [term()]) -> context_ref().
info(ContextRef, Format, Args) -> info_opt(ContextRef, Format, Args, []).

-spec info_opt(io:format(), log_options()) -> context_ref().
info_opt(Format, Options) -> info_opt(Format, [], Options).

-spec info_opt(io:format(), [term()], log_options()) -> context_ref().
info_opt(Format, Args, Options) -> info_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec info_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
info_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, info, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec notice(io:format()) -> context_ref().
notice(Format) -> notice(Format, []).

-spec notice(io:format(), [term()]) -> context_ref().
notice(Format, Args) -> notice(?DEFAULT_LOGGER, Format, Args).

-spec notice(context_ref(), io:format(), [term()]) -> context_ref().
notice(ContextRef, Format, Args) -> notice_opt(ContextRef, Format, Args, []).

-spec notice_opt(io:format(), log_options()) -> context_ref().
notice_opt(Format, Options) -> notice_opt(Format, [], Options).

-spec notice_opt(io:format(), [term()], log_options()) -> context_ref().
notice_opt(Format, Args, Options) -> notice_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec notice_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
notice_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, notice, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec warning(io:format()) -> context_ref().
warning(Format) -> warning(Format, []).

-spec warning(io:format(), [term()]) -> context_ref().
warning(Format, Args) -> warning(?DEFAULT_LOGGER, Format, Args).

-spec warning(context_ref(), io:format(), [term()]) -> context_ref().
warning(ContextRef, Format, Args) -> warning_opt(ContextRef, Format, Args, []).

-spec warning_opt(io:format(), log_options()) -> context_ref().
warning_opt(Format, Options) -> warning_opt(Format, [], Options).

-spec warning_opt(io:format(), [term()], log_options()) -> context_ref().
warning_opt(Format, Args, Options) -> warning_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec warning_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
warning_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, warning, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec error(io:format()) -> context_ref().
error(Format) -> error(Format, []).

-spec error(io:format(), [term()]) -> context_ref().
error(Format, Args) -> error(?DEFAULT_LOGGER, Format, Args).

-spec error(context_ref(), io:format(), [term()]) -> context_ref().
error(ContextRef, Format, Args) -> error_opt(ContextRef, Format, Args, []).

-spec error_opt(io:format(), log_options()) -> context_ref().
error_opt(Format, Options) -> error_opt(Format, [], Options).

-spec error_opt(io:format(), [term()], log_options()) -> context_ref().
error_opt(Format, Args, Options) -> error_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec error_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
error_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, error, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec critical(io:format()) -> context_ref().
critical(Format) -> critical(Format, []).

-spec critical(io:format(), [term()]) -> context_ref().
critical(Format, Args) -> critical(?DEFAULT_LOGGER, Format, Args).

-spec critical(context_ref(), io:format(), [term()]) -> context_ref().
critical(ContextRef, Format, Args) -> critical_opt(ContextRef, Format, Args, []).

-spec critical_opt(io:format(), log_options()) -> context_ref().
critical_opt(Format, Options) -> critical_opt(Format, [], Options).

-spec critical_opt(io:format(), [term()], log_options()) -> context_ref().
critical_opt(Format, Args, Options) -> critical_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec critical_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
critical_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, critical, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec alert(io:format()) -> context_ref().
alert(Format) -> alert(Format, []).

-spec alert(io:format(), [term()]) -> context_ref().
alert(Format, Args) -> alert(?DEFAULT_LOGGER, Format, Args).

-spec alert(context_ref(), io:format(), [term()]) -> context_ref().
alert(ContextRef, Format, Args) -> alert_opt(ContextRef, Format, Args, []).

-spec alert_opt(io:format(), log_options()) -> context_ref().
alert_opt(Format, Options) -> alert_opt(Format, [], Options).

-spec alert_opt(io:format(), [term()], log_options()) -> context_ref().
alert_opt(Format, Args, Options) -> alert_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec alert_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
alert_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, alert, logi_location:make(undefined, undefined, 0), Format, Args, Options).

-spec emergency(io:format()) -> context_ref().
emergency(Format) -> emergency(Format, []).

-spec emergency(io:format(), [term()]) -> context_ref().
emergency(Format, Args) -> emergency(?DEFAULT_LOGGER, Format, Args).

-spec emergency(context_ref(), io:format(), [term()]) -> context_ref().
emergency(ContextRef, Format, Args) -> emergency_opt(ContextRef, Format, Args, []).

-spec emergency_opt(io:format(), log_options()) -> context_ref().
emergency_opt(Format, Options) -> emergency_opt(Format, [], Options).

-spec emergency_opt(io:format(), [term()], log_options()) -> context_ref().
emergency_opt(Format, Args, Options) -> emergency_opt(?DEFAULT_LOGGER, Format, Args, Options).

-spec emergency_opt(context_ref(), io:format(), [term()], log_options()) -> context_ref().
emergency_opt(ContextRef, Format, Args, Options) ->
    log(ContextRef, emergency, logi_location:make(undefined, undefined, 0), Format, Args, Options).
