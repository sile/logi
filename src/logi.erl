%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         is_log_level/1
        ]).

-export([
         start_event_manager/0, start_event_manager/1,
         stop_event_manager/1,
         which_event_managers/0,

         add_handler/2, add_handler/3,
         delete_handler/1, delete_handler/2,
         which_handlers/0, which_handlers/1

         %% log/3, log/4,
         %% set_loglevel/2, set_loglevel/3
        ]).

-export([load_context/0, save_context/1]).
-export([set_header/1, set_header/2,
         unset_header/1, unset_header/2,
         erase_header/0, erase_header/1,
         get_header/0, get_header/1]).
-export([set_metadata/1, set_metadata/2,
         unset_metadata/1, unset_metadata/2,
         erase_metadata/0, erase_metadata/1,
         get_metadata/0, get_metadata/1]).

-export([log_levels/0, log_level_to_int/1, upper_log_levels/1]).

-export_type([backend/0,
              backend_ref/0,
              backend_id/0,
              backend_options/0,
              log_level/0,
              conditions/0]).

-export_type([event_manager_name/0, % XXX: event
              event_manager_ref/0,
              event_handler/0]).

-export_type([context/0]).

-export_type([header_entry/0, header_entry_key/0, header_entry_value/0,
              header_scope/0, header_option/0]).
              
-export_type([metadata_entry/0, metadata_entry_key/0, metadata_entry_value/0,
              metadata_option/0]).

-export_type([exception_reason/0, exception_class/0, stacktrace/0]).

-export_type([frequency_policy/0, severity/0]).

-export_type([condition_spec/0, condition_clause/0, condition/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(FOREACH_HEADER_SCOPE_FUN(Fun, Options),
        fun (_Header) -> lists:foldl(Fun, _Header, logi_util_assoc:fetch(scope, Options, [process])) end).

-type condition_spec() :: condition_clause() | [condition_clause()].
-type condition_clause() :: log_level() | {log_level(), condition()}.
-type condition() :: always
                   | {match, {module(), Function::atom(), Pattern::term()}}.

-type backend() :: logi_backend:backend().
-type backend_ref() :: pid() | atom().
-type backend_id() :: term().
-type backend_options() :: term().
-type log_level() :: severity().
-type conditions() :: [metadata_entry()].

-type severity() :: debug | verbose | info | warning | alert. % TODO:

-type frequency_policy() :: always
                          | once
                          | {interval_count, non_neg_integer()}
                          | {interval_time, timeout()}.

-type event_manager_name() :: {local, Name::atom()}
                            | {global, GlobalName::term()}
                            | {via, module(), ViaName::term()}.

%% TODO: atom() | pid() に限定する
-type event_manager_ref() :: atom()
                           | {atom(), node()}
                           | {global, term()}
                           | {via, module(), term()}
                           | pid().

-type event_handler() :: module()
                       | {module(), Id::term()}.

-opaque context() :: [{'LOGI_MSG_HEADER_CONTEXT',   log_msg_context:context()} |
                      {'LOGI_MSG_METADATA_CONTEXT', log_msg_context:context()}].

-type header_entry()       :: {header_entry_key(), header_entry_value()}.
-type header_entry_key()   :: term().
-type header_entry_value() :: term().
-type header_scope()       :: metadata_entry().

-type header_option() :: {scope, [header_scope()]}
                       | {manager, logi:event_manager_ref()}.

-type metadata_entry()       :: {metadata_entry_key(), metadata_entry_value()}.
-type metadata_entry_key()   :: term().
-type metadata_entry_value() :: term().

-type metadata_option() :: {manager, event_manager_ref()}.

-type exception_reason() :: {'EXCEPTION', exception_class(), stacktrace()}.
-type exception_class() :: exit | error | throw.
-type stacktrace() :: [erlang:stack_item()].

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_log_level(log_level() | term()) -> boolean().
is_log_level(X) -> lists:member(X, log_levels()).

-spec log_levels() -> [log_level()].
log_levels() -> [debug, verbose, info, warning, alert].

-spec upper_log_levels(log_level()) -> [log_level()].
upper_log_levels(Level) -> 
    lists:dropwhile(fun (L) -> L =/= Level end, log_levels()).

-spec log_level_to_int(log_level()) -> non_neg_integer().
log_level_to_int(debug) -> 0;
log_level_to_int(verbose) -> 1;
log_level_to_int(info) ->  2;
log_level_to_int(warning) ->  3;
log_level_to_int(alert) -> 4.

%% @doc TODO
-spec start_event_manager() -> {ok, pid()} | {error, Reason::term()}.
start_event_manager() ->
    logi_event_manager_sup:start_event_manager().

%% @doc TODO
-spec start_event_manager(event_manager_name()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_event_manager(ManagerName) ->
    logi_event_manager_sup:start_event_manager(ManagerName).

%% @doc TODO
-spec which_event_managers() -> [pid()].
which_event_managers() ->
    logi_event_manager_sup:which_event_managers().

%% @doc TODO
-spec stop_event_manager(event_manager_ref()) -> ok.
stop_event_manager(ManagerRef) ->
    logi_event_manager_sup:stop_event_manager(ManagerRef).

%% @equiv add_handler(?LOGI_DEFAULT_EVENT_MANAGER, Handler, Args)
-spec add_handler(event_handler(), Args) -> ok | {error, Reason} when
      Args   :: [term()],
      Reason :: already_exists | exception_reason() | term().
add_handler(Handler, Args) ->
    add_handler(?LOGI_DEFAULT_EVENT_MANAGER, Handler, Args).

%% @doc TODO
-spec add_handler(event_manager_ref(), event_handler(), Args) -> ok | {error, Reason} when
      Args   :: [term()],
      Reason :: already_exists | exception_reason() | term().
add_handler(ManagerRef, Handler, Args) ->
    logi_event_manager:add_handler(ManagerRef, Handler, Args).

%% @equiv delete_handler(?LOGI_DEFAULT_EVENT_MANAGER, Handler)
-spec delete_handler(event_handler()) -> ok | {error, module_not_found} | exception_reason().
delete_handler(Handler) ->
    delete_handler(?LOGI_DEFAULT_EVENT_MANAGER, Handler).

%% @doc TODO
-spec delete_handler(event_manager_ref(), event_handler()) -> ok | {error, module_not_found} | exception_reason().
delete_handler(ManagerRef, Handler) ->
    logi_event_manager:delete_handler(ManagerRef, Handler).

%% @equiv which_handlers(?LOGI_DEFAULT_EVENT_MANAGER)
-spec which_handlers() -> [event_handler()].
which_handlers() ->
    which_handlers(?LOGI_DEFAULT_EVENT_MANAGER).

%% @doc TODO
-spec which_handlers(event_manager_ref()) -> [event_handler()].
which_handlers(ManagerRef) ->
    logi_event_manager:which_handlers(ManagerRef).

%% @doc 現在のプロセスのログ出力のコンテキスト情報を取得する
-spec load_context() -> context().
load_context() ->
    [
     {'LOGI_MSG_HEADER_CONTEXT',   logi_msg_context:load_context(logi_msg_header)},
     {'LOGI_MSG_METADATA_CONTEXT', logi_msg_context:load_context(logi_msg_metadata)}
    ].

%% @doc 現在のプロセスにログ出力のコンテキスト情報を保存(上書き)する
-spec save_context(context()) -> ok.
save_context(Context) when is_list(Context) ->
    ok = logi_msg_context:save_context(logi_msg_header, logi_util_assoc:fetch('LOGI_MSG_HEADER_CONTEXT', Context, [])),
    ok = logi_msg_context:save_context(logi_msg_metadata, logi_util_assoc:fetch('LOGI_MSG_METADATA_CONTEXT', Context, [])),
    ok.

%% @equiv set_header(HeaderEntries, [])
-spec set_header([header_entry()]) -> ok.
set_header(HeaderEntries) -> set_header(HeaderEntries, []).

%% @doc ヘッダ情報を設定する
%%
%% 既に存在するキーが指定された場合は、値が上書きされる
-spec set_header([header_entry()], [header_option()]) -> ok.
set_header(HeaderEntries, Options) ->
    Fun = ?FOREACH_HEADER_SCOPE_FUN(fun (Scope, Header) -> logi_msg_header:set_entries(Scope, HeaderEntries, Header) end, Options),
    _ = logi_msg_context:update_info(get_event_manager(Options), Fun, logi_msg_header),
    ok.

%% @equiv unset_header(HeaderEntries, [])
-spec unset_header([header_entry_key()]) -> ok.
unset_header(HeaderEntryKeys) -> unset_header(HeaderEntryKeys, []).

%% @doc ヘッダ情報から指定されたエントリを削除する
-spec unset_header([header_entry_key()], [header_option()]) -> ok.
unset_header(HeaderEntryKeys, Options) ->
    Fun = ?FOREACH_HEADER_SCOPE_FUN(fun (Scope, Header) -> logi_msg_header:unset_entries(Scope, HeaderEntryKeys, Header) end, Options),
    _ = logi_msg_context:update_info(get_event_manager(Options), Fun, logi_msg_header),
    ok.

%% @equiv erase_header([])
-spec erase_header() -> ok.
erase_header() -> erase_header([]).

%% @doc ヘッダ情報を削除する
-spec erase_header([header_option()]) -> ok.
erase_header(Options) ->
    Fun = ?FOREACH_HEADER_SCOPE_FUN(fun (Scope, Header) -> logi_msg_header:erase_entries(Scope, Header) end, Options),
    _ = logi_msg_context:update_info(get_event_manager(Options), Fun, logi_msg_header),
    ok.

%% @equiv get_header([]) -> ok.
-spec get_header() -> [header_entry()].
get_header() -> get_header([]).

%% @doc ヘッダ情報を取得する
-spec get_header([header_option()]) -> [header_entry()].
get_header(Options) ->
    ScopeList = lists:ukeymerge(1, lists:ukeysort(1, logi_util_assoc:fetch(scope, Options, [])), get_metadata(Options)),
    Header = logi_msg_context:get_info(get_event_manager(Options), logi_msg_header),
    logi_msg_header:get_entries(ScopeList, Header).

%% @equiv set_metadata(MetaDataEntries, [])
-spec set_metadata([metadata_entry()]) -> ok.
set_metadata(MetaDataEntries) -> set_metadata(MetaDataEntries, []).

%% @doc メタデータ情報を設定する
%%
%% 既に存在するキーが指定された場合は、値が上書きされる
-spec set_metadata([metadata_entry()], [metadata_option()]) -> ok.
set_metadata(MetaDataEntries, Options) ->
    Fun = fun (MetaData) -> logi_msg_metadata:set_entries(MetaDataEntries, MetaData) end,
    _ = logi_msg_context:update_info(get_event_manager(Options), Fun, logi_msg_metadata),
    ok.

%% @equiv unset_metadata(MetaDataEntries, [])
-spec unset_metadata([metadata_entry_key()]) -> ok.
unset_metadata(MetaDataEntryKeys) -> unset_metadata(MetaDataEntryKeys, []).

%% @doc メタデータ情報から指定されたエントリを削除する
-spec unset_metadata([metadata_entry_key()], [metadata_option()]) -> ok.
unset_metadata(MetaDataEntryKeys, Options) ->
    Fun = fun (MetaData) -> logi_msg_metadata:unset_entries(MetaDataEntryKeys, MetaData) end,
    _ = logi_msg_context:update_info(get_event_manager(Options), Fun, logi_msg_metadata),
    ok.

%% @equiv erase_metadata([])
-spec erase_metadata() -> ok.
erase_metadata() -> erase_metadata([]).

%% @doc メタデータ情報を削除する
-spec erase_metadata([metadata_option()]) -> ok.
erase_metadata(Options) ->
    Fun = fun (_MetaData) -> logi_msg_metadata:empty() end,
    _ = logi_msg_context:update_info(get_event_manager(Options), Fun, logi_msg_metadata),
    ok.

%% @equiv get_metadata([]) -> ok.
-spec get_metadata() -> [metadata_entry()].
get_metadata() -> get_metadata([]).

%% @doc メタデータ情報を取得する
-spec get_metadata([metadata_option()]) -> [metadata_entry()].
get_metadata(Options) ->
    MetaData = logi_msg_context:get_info(get_event_manager(Options), logi_msg_metadata),
    logi_msg_metadata:get_entries(MetaData).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec get_event_manager(Options) -> event_manager_ref() when
      Options :: [{manager, event_manager_ref()}].
get_event_manager(Options) ->
    logi_util_assoc:fetch(manager, Options, ?LOGI_DEFAULT_EVENT_MANAGER).
