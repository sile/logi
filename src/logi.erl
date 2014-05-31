%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         default_backend_manager/0,
         start_backend_manager/1,
         stop_backend_manager/1,
         which_backend_managers/0,

         add_backend/2, add_backend/3,
         delete_backend/1, delete_backend/2,
         which_backends/0, which_backends/1,

         log_levels/0,
         log/5, log/6,

         make_context/0, make_context/1,
         save_context/1, save_context/2,
         load_context/0, load_context/1,
         which_contexts/0,

         set_headers/1, set_headers/2,
         get_headers/0, get_headers/1,
         update_headers/1, update_headers/2,
         delete_headers/1, delete_headers/2,
         set_metadata/1, set_metadata/2,
         get_metadata/0,  get_metadata/1,
         update_metadata/1, update_metadata/2,
         delete_metadata/1, delete_metadata/2
        ]).

-export_type([
              backend_manager/0,
              location/0,
              metadata/0,
              format_metadata/0,
              headers/0,
              log_options/0,
              log_option/0,
              frequency_policy_spec/0
             ]).

-export_type([backend/0,
              backend_ref/0,
              backend_id/0,
              backend_data/0,
              log_level/0]).

-export_type([condition_spec/0, condition_clause/0, condition/0]).
-export_type([context/0, context_id/0, context_ref/0]).
-export_type([header_entry/0, header_entry_key/0, header_entry_value/0]).
-export_type([metadata_entry/0, metadata_entry_key/0, metadata_entry_value/0]).
-export_type([stacktrace/0]).
-export_type([frequency_policy/0, severity/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(LOGI_DEFAULT_BACKEND_MANAGER, logi_default_backend_manager).
-define(LOGI_CONTEXT_TAG, '__LOGI_CONTEXT__').

-define(WITH_CONTEXT(ContextRef, Fun),
        case logi_context:is_context(ContextRef) of
            true  -> (Fun)(ContextRef);
            false -> ok = save_context(ContextRef, (Fun)(load_context(ContextRef))), ContextRef
        end).

-define(WITH_READ_CONTEXT(ContextRef, Fun),
        case logi_context:is_context(ContextRef) of
            true  -> (Fun)(ContextRef);
            false -> (Fun)(load_context(ContextRef))
        end).

-type backend_manager()          :: atom().
-type context()                  :: logi:context().
-type context_id()               :: term().
-type context_ref()              :: context() | context_id().
-type location()                 :: logi_location:location().
-type metadata() :: [metadata_entry()].
-type format_metadata() :: [metadata_entry() | 
                            {process, pid()} |
                            {node, node()} |
                            {application, atom()} |
                            {module, module()} |
                            {function, atom()} |
                            {line, pos_integer()}].

-type headers()  :: [header_entry()].

-type log_options() :: [log_option()].
-type log_option() :: {context, context_ref()}
                    | {headers, headers()}
                    | {metadata, metadata()}
                    | {frequency, frequency_policy_spec()}.

-type frequency_policy_spec() :: always
                               | once
                               | {once_in_times, Times::pos_integer()}
                               | {interval, MilliSeconds::non_neg_integer()}.


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
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency.

-type severity() :: log_level().

-type frequency_policy() :: always
                          | once
                          | {interval_count, non_neg_integer()}
                          | {interval_time, timeout()}.


-type header_entry()       :: {header_entry_key(), header_entry_value()}.
-type header_entry_key()   :: term().
-type header_entry_value() :: term().

-type metadata_entry()       :: {metadata_entry_key(), metadata_entry_value()}.
-type metadata_entry_key()   :: term().
-type metadata_entry_value() :: term().

-type stacktrace() :: [erlang:stack_item()].

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec default_backend_manager() -> backend_manager().
default_backend_manager() ->
    ?LOGI_DEFAULT_BACKEND_MANAGER.
    
-spec start_backend_manager(backend_manager()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_backend_manager(ManagerId) when is_atom(ManagerId) -> logi_backend_manager_sup:start_manager(ManagerId);
start_backend_manager(ManagerId)                         -> error(badarg, [ManagerId]).

-spec stop_backend_manager(backend_manager()) -> ok.
stop_backend_manager(ManagerId) when is_atom(ManagerId) -> logi_backend_manager_sup:stop_manager(ManagerId);
stop_backend_manager(ManagerId)                         -> error(badarg, [ManagerId]).

-spec which_backend_managers() -> [backend_manager()].
which_backend_managers() -> logi_backend_manager_sup:which_managers().

-spec add_backend(condition_spec(), backend_spec()) -> ok | {error, Reason} when
      Reason :: {already_exists, backend()}.
add_backend(ConditionSpec, BackendSpec) ->
    add_backend(?LOGI_DEFAULT_BACKEND_MANAGER, ConditionSpec, BackendSpec).

-spec add_backend(backend_manager(), condition_spec(), backend_spec()) -> ok | {error, Reason} when
      Reason :: {already_exists, backend()}.
add_backend(ManagerId, ConditionSpec, BackendSpec) ->
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

-spec delete_backend(backend_manager(), backend_id()) -> ok | {error, not_found}.
delete_backend(ManagerId, BackendId) ->
    logi_backend_manager:delete_backend(ManagerId, BackendId).

-spec which_backends() -> [logi:backend()].
which_backends() ->
    which_backends(?LOGI_DEFAULT_BACKEND_MANAGER).

-spec which_backends(backend_manager()) -> [logi:backend()].
which_backends(ManagerId) ->
    logi_backend_manager:which_backends(ManagerId).

-spec log_levels() -> [log_level()].
log_levels() -> [debug, verbose, info, notice, warning, error, critical, alert, emergency].

-spec log(severity(), location(), io:format(), [term()], log_options()) -> context_ref().
log(Severity, Location, Format, Args, Options) ->
    log(?LOGI_DEFAULT_BACKEND_MANAGER, Severity, Location, Format, Args, Options).

-spec log(backend_manager(), severity(), location(), io:format(), [term()], log_options()) -> context_ref().
log(Manager, Severity, Location, Format, Args, Options) ->
    ContextRef = logi_util_assoc:fetch(context, Options, Manager),
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) -> logi_client:log(Manager, Severity, Location, Format, Args, Options, Context) end).

-spec make_context() -> context().
make_context() ->
    logi_context:make().

-spec make_context(Options) -> context() when
      Options :: [Option],
      Option  :: {headers, headers()} | {metadata, metadata()}.
make_context(Options) ->
    logi_context:make(logi_util_assoc:fetch(headers, Options, []),
                      logi_util_assoc:fetch(metadata, Options, [])).

-spec save_context(context()) -> ok.
save_context(Context) ->
    save_context(?LOGI_DEFAULT_BACKEND_MANAGER, Context).

-spec save_context(context_id(), context()) -> ok.
save_context(ContextId, Context) ->
    case logi_context:is_context(Context) of
        false -> error(badarg, [ContextId, Context]);
        true  -> _ = put({?LOGI_CONTEXT_TAG, ContextId}, Context), ok
    end.

-spec load_context() -> context().
load_context() ->
    load_context(?LOGI_DEFAULT_BACKEND_MANAGER).

-spec load_context(context_id()) -> context().
load_context(ContextId) ->
    case get({?LOGI_CONTEXT_TAG, ContextId}) of
        undefined -> make_context();
        Context   -> Context
    end.

-spec which_contexts() -> [{context_id(), context()}].
which_contexts() ->
    [{Id, Context} || {{?LOGI_CONTEXT_TAG, Id}, Context} <- get()].

-spec set_headers(headers()) -> context_id().
set_headers(Headers) ->
    set_headers(?LOGI_DEFAULT_BACKEND_MANAGER, Headers).

-spec set_headers(context_ref(), headers()) -> context_ref().
set_headers(ContextRef, Headers) ->
    ok = logi_util_assoc:assert_assoc_list(Headers),
    ?WITH_CONTEXT(ContextRef, fun (Context) -> logi_context:set_headers(lists:ukeysort(1, Headers), Context) end).

-spec get_headers() -> headers().
get_headers() ->
    get_headers(?LOGI_DEFAULT_BACKEND_MANAGER).

-spec get_headers(context_ref()) -> headers().
get_headers(ContextRef) ->
    ?WITH_READ_CONTEXT(ContextRef, fun logi_context:get_headers/1).

-spec update_headers(headers()) -> context_id().
update_headers(Headers) ->
    update_headers(?LOGI_DEFAULT_BACKEND_MANAGER, Headers).

-spec update_headers(context_ref(), headers()) -> context_ref().
update_headers(ContextRef, Headers) ->
    ok = logi_util_assoc:assert_assoc_list(Headers),
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          Merged = lists:ukeymerge(1, lists:ukeysort(1, Headers), get_headers(Context)),
                          logi_context:set_headers(Merged, Context)
                  end).

-spec delete_headers([header_entry_key()]) -> context_id().
delete_headers(Keys) ->
    delete_backend(?LOGI_DEFAULT_BACKEND_MANAGER, Keys).

-spec delete_headers(context_ref(), [header_entry_key()]) -> context_ref().
delete_headers(ContextRef, Keys) ->
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          Headers0 = get_headers(Context),
                          Headers1 = lists:foldl(fun (Key, Acc) -> lists:keydelete(Key, 1, Acc) end, Keys, Headers0),
                          logi_context:set_headers(Headers1, Context)
                  end).

-spec set_metadata(metadata()) -> context_id().
set_metadata(MetaData) ->
    set_metadata(?LOGI_DEFAULT_BACKEND_MANAGER, MetaData).

-spec set_metadata(context_ref(), metadata()) -> context_ref().
set_metadata(ContextRef, MetaData) ->
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          ok = logi_util_assoc:assert_assoc_list(MetaData),
                          logi_context:set_metadata(lists:ukeysort(1, MetaData), Context)
                  end).

-spec get_metadata() -> metadata().
get_metadata() ->
    get_metadata(?LOGI_DEFAULT_BACKEND_MANAGER).

-spec get_metadata(context_ref()) -> metadata().
get_metadata(ContextRef) ->
    ?WITH_READ_CONTEXT(ContextRef, fun logi_context:get_metadata/1).

-spec update_metadata(metadata()) -> context_id().
update_metadata(MetaData) ->
    update_metadata(MetaData, ?LOGI_DEFAULT_BACKEND_MANAGER).

-spec update_metadata(context_ref(), metadata()) -> context_ref().
update_metadata(ContextRef, MetaData) ->
    ok = logi_util_assoc:assert_assoc_list(MetaData),
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          Merged = lists:ukeymerge(1, lists:ukeysort(1, MetaData), get_metadata(Context)),
                          logi_context:set_metadata(Merged, Context)
                  end).

-spec delete_metadata([header_entry_key()]) -> context_id().
delete_metadata(Keys) ->
    delete_metadata(?LOGI_DEFAULT_BACKEND_MANAGER, Keys).

-spec delete_metadata(context_ref(), [header_entry_key()]) -> context_ref().
delete_metadata(ContextRef, Keys) ->
    ?WITH_CONTEXT(ContextRef,
                  fun (Context) ->
                          MetaData0 = get_metadata(Context),
                          MetaData1 = lists:foldl(fun (Key, Acc) -> lists:keydelete(Key, 1, Acc) end, Keys, MetaData0),
                          logi_context:set_metadata(MetaData1, Context)
                  end).
