%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start_backend_manager/1,
         stop_backend_manager/1,
         which_backend_managers/0,

         add_backend/2, add_backend/3,
         delete_backend/1, delete_backend/2,
         which_backends/0, which_backends/1,

         log/5,

         load_process_state/0,
         save_process_state/1,
         copy_process_state/1
        ]).

-export([set_header/1, set_header/2,
         unset_header/1, unset_header/2,
         erase_header/0, erase_header/1,
         get_header/0, get_header/1]).
-export([set_metadata/1, set_metadata/2,
         unset_metadata/1, unset_metadata/2,
         erase_metadata/0, erase_metadata/1,
         get_metadata/0, get_metadata/1]).

-export_type([backend/0,
              backend_ref/0,
              backend_id/0,
              backend_options/0,
              log_level/0,
              conditions/0]).

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

-type process_state() :: term(). % TODO:

-type backend_data() :: term().
-type backend_spec() :: {backend_ref(), module(), backend_data()}
                      | {backend_id(), backend_ref(), module(), backend_data()}.

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

-type backend_manager_id() :: atom().

-opaque context() :: [{'LOGI_MSG_HEADER_CONTEXT',   log_msg_context:context()} |
                      {'LOGI_MSG_METADATA_CONTEXT', log_msg_context:context()}].

-type header_entry()       :: {header_entry_key(), header_entry_value()}.
-type header_entry_key()   :: term().
-type header_entry_value() :: term().
-type header_scope()       :: metadata_entry().

-type header_option() :: {scope, [header_scope()]}
                       | {manager, backend_manager_id()}.

-type metadata_entry()       :: {metadata_entry_key(), metadata_entry_value()}.
-type metadata_entry_key()   :: term().
-type metadata_entry_value() :: term().

-type metadata_option() :: {manager, backend_manager_id()}.

-type exception_reason() :: {'EXCEPTION', exception_class(), stacktrace()}.
-type exception_class() :: exit | error | throw.
-type stacktrace() :: [erlang:stack_item()].

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec start_backend_manager(backend_manager_id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_backend_manager(ManagerId) when is_atom(ManagerId) -> logi_backend_manager_sup:start_manager(ManagerId);
start_backend_manager(ManagerId)                         -> error(badarg, [ManagerId]).

-spec stop_backend_manager(backend_manager_id()) -> ok.
stop_backend_manager(ManagerId) when is_atom(ManagerId) -> logi_backend_manager_sup:stop_manager(ManagerId);
stop_backend_manager(ManagerId)                         -> error(badarg, [ManagerId]).

-spec which_backend_managers() -> [backend_manager_id()].
which_backend_managers() -> logi_backend_manager_sup:which_managers().

-spec add_backend(backend_spec(), condition_spec()) -> ok | {error, Reason} when
      Reason :: {already_exists, backend()}.
add_backend(BackendSpec, ConditionSpec) ->
    add_backend(?LOGI_DEFAULT_BACKEND_MANAGER, BackendSpec, ConditionSpec).

-spec add_backend(backend_manager_id(), backend_spec(), condition_spec()) -> ok | {error, Reason} when
      Reason :: {already_exists, backend()}.
add_backend(ManagerId, BackendSpec, ConditionSpec) ->
    Condition = logi_condition:make(ConditionSpec),
    Backend =
        case BackendSpec of
            {Ref, Module, Data}     -> logi_backend:make(Ref, Module, Condition, Data);
            {Id, Ref, Module, Data} -> logi_backend:make(Id, Ref, Module, Condition, Data)
        end,
    logi_backend_manager:add_backend(ManagerId, Backend).

-spec delete_backend(backend_id()) -> ok | {error, not_found}.
delete_backend(BackendId) ->
    delete_backend(?LOGI_DEFAULT_BACKEND_MANAGER, BackendId).

-spec delete_backend(backend_manager_id(), backend_id()) -> ok | {error, not_found}.
delete_backend(ManagerId, BackendId) ->
    logi_backend_manager:delete_backend(ManagerId, BackendId).

-spec which_backends() -> [logi:backend()].
which_backends() ->
    which_backends(?LOGI_DEFAULT_BACKEND_MANAGER).

-spec which_backends(backend_manager_id()) -> [logi:backend()].
which_backends(ManagerId) ->
    logi_backend_manager:which_backends(ManagerId).

-spec log(severity(), string()|binary()|atom(), [term()], #logi_log_context{}, #logi_log_option{}) -> ok.
log(Severity, Format, Args, Context, Options) ->
    logi_client:log(Severity, Format, Args, Context, Options).

-spec load_process_state() -> process_state().
load_process_state() ->
    [
     {'LOGI_MSG_HEADER_CONTEXT',   logi_msg_context:load_context(logi_msg_header)},
     {'LOGI_MSG_METADATA_CONTEXT', logi_msg_context:load_context(logi_msg_metadata)}
    ].

-spec save_process_state(process_state()) -> ok.
save_process_state(ProcessState) ->
    ok = logi_msg_context:save_context(logi_msg_header, logi_util_assoc:fetch('LOGI_MSG_HEADER_CONTEXT', ProcessState, [])),
    ok = logi_msg_context:save_context(logi_msg_metadata, logi_util_assoc:fetch('LOGI_MSG_METADATA_CONTEXT', ProcessState, [])),
    ok.

-spec copy_process_state(pid()) -> ok.
copy_process_state(Pid) ->
    save_process_state(process_info(Pid, dictionary)).

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
-spec get_event_manager(Options) -> backend_manager_id() when
      Options :: [{manager, backend_manager_id()}].
get_event_manager(Options) ->
    logi_util_assoc:fetch(manager, Options, ?LOGI_DEFAULT_BACKEND_MANAGER).
