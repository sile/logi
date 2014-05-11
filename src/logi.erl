%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start_event_manager/0, start_event_manager/1,
         stop_event_manager/1,
         which_event_managers/0,

         add_handler/2, add_handler/3,
         delete_handler/1, delete_handler/2,
         which_handlers/0, which_handlers/1,

         set_header/1, set_header/2,
         unset_header/1, unset_header/2,
         get_header/0, get_header/1

         %% set_metadata/1, set_metadata/2,
         %% unset_metadta/1, unset_metadta/2,
         %% get_metadata/0, get_metadata/1

         %% set_local_header/1, set_local_header/2,
         %% add_local_header/1, add_local_header/2,
         %% log/3, log/4,
         %% set_loglevel/2, set_loglevel/3
        ]).

-export_type([event_manager_name/0, % XXX: event
              event_manager_ref/0,
              event_handler/0,

              header_entry/0,
              header_entry_key/0,
              header_scope/0,
              set_header_option/0,
              unset_header_option/0,
              get_header_option/0,
              header_state/0,

              metadata/0,
              metadata_entry/0,
              metadata_option/0,

              exception_reason/0,
              exception_class/0,
              stacktrace/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type event_manager_name() :: {local, Name::atom()}
                            | {global, GlobalName::term()}
                            | {via, module(), ViaName::term()}.

%% TODO: atom() | pid() に限定する
-type event_manager_ref() :: atom()
                           | {atom(), node()}
                           | {global, term()}
                           | {via, module(), term()}
                           | pid().

-type header_entry() :: {header_entry_key(), HeaderValue::term()}.
-type header_entry_key() :: atom().
-type set_header_option() :: {scope, header_scope()}
                           | {manager, logi:event_manager_ref()}.
-type unset_header_option() :: set_header_option().
-type get_header_option() :: {metadata, [metadata_entry()]}
                           | {manager, event_manager_ref()}.

-type header_scope() :: {MetaDataKey::term(), MetaDataValue::term()}.

-opaque header_state() :: logi_client:whole_header_state().

-opaque metadata() :: todo.

-type metadata_entry() :: {atom(), term()}.

-type metadata_option() :: {manager, event_manager_ref()}.

-type event_handler() :: module()
                       | {module(), Id::term()}.

-type exception_reason() :: {'EXCEPTION', exception_class(), stacktrace()}.
-type exception_class() :: exit | error | throw.
-type stacktrace() :: [erlang:stack_item()].

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
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

%% TODO: event_manager指定はOptionsの中に含めて、set_header/1を用意する
%% @equiv set_header(HeaderEntries, [])
-spec set_header([header_entry()]) -> ok.
set_header(HeaderEntries) ->
    set_header(HeaderEntries, []).

%% @doc TODO
-spec set_header([header_entry()], [set_header_option()]) -> ok.
set_header(HeaderEntries, _Options) ->
    ManagerRef = todo,
    Scope = process,
    Fun = fun (Header) -> logi_msg_header:set_entries(Scope, HeaderEntries, Header) end,
    _ = logi_msg_context:update_info(ManagerRef, Fun, logi_msg_header),
    ok.

%% @equiv unset_header(HeaderEntries, [])
-spec unset_header([header_entry_key()]) -> ok.
unset_header(HeaderEntryKeys) ->
    unset_header(HeaderEntryKeys, []).

%% @doc TODO
-spec unset_header([header_entry_key()], [unset_header_option()]) -> ok.
unset_header(HeaderEntryKeys, _Options) ->
    ManagerRef = todo,
    Scope = process,
    Fun = fun (Header) -> logi_msg_header:unset_entries(Scope, HeaderEntryKeys, Header) end,
    _ = logi_msg_context:update_info(ManagerRef, Fun, logi_msg_header),
    ok.

%% @equiv get_header([]) -> ok.
-spec get_header() -> [header_entry()].
get_header() ->
    get_header([]).

%% @doc TODO
-spec get_header([get_header_option()]) -> [header_entry()].
get_header(_Options) ->
    ManagerRef = todo,
    ScopeList = [],
    Header = logi_msg_context:get_info(ManagerRef, logi_msg_header),
    logi_msg_header:get_entries(ScopeList, Header).
