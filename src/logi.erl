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

         set_header/2, set_header/3,
         unset_header/2, unset_header/3,
         get_header/1, get_header/2

         %% set_header/1, set_header/2,
         %% add_header/1, add_header/2,
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
-type set_header_option() :: {scope, header_scope()}. %% NOTE: functionスコープはmacroで実装する ?LOGI_WITH_HEADER
-type unset_header_option() :: set_header_option().
-type get_header_option() :: {metadata, [metadata_entry()]}.

-type header_scope() :: {MetaDataKey::atom(), MetaDataValue::term()}.

-opaque header_state() :: logi_client:whole_header_state().

-opaque metadata() :: todo.

-type metadata_entry() :: {atom(), term()}.

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

%% @equiv set_header(?LOGI_DEFAULT_EVENT_MANAGER, HeaderEntries, Options)
-spec set_header([header_entry()], [set_header_option()]) -> ok.
set_header(HeaderEntries, Options) ->
    set_header(?LOGI_DEFAULT_EVENT_MANAGER, HeaderEntries, Options).

%% @doc TODO
-spec set_header(event_manager_ref(), [header_entry()], [set_header_option()]) -> ok.
set_header(ManagerRef, HeaderEntries, Options) ->
    logi_msg_header:set_header(ManagerRef, HeaderEntries, Options).

%% @equiv unset_header(?LOGI_DEFAULT_EVENT_MANAGER, HeaderEntries, Options)
-spec unset_header([header_entry_key()], [unset_header_option()]) -> ok.
unset_header(HeaderEntryKeys, Options) ->
    unset_header(?LOGI_DEFAULT_EVENT_MANAGER, HeaderEntryKeys, Options).

%% @doc TODO
-spec unset_header(event_manager_ref(), [header_entry_key()], [unset_header_option()]) -> ok.
unset_header(ManagerRef, HeaderEntryKeys, Options) ->
    logi_msg_header:unset_header(ManagerRef, HeaderEntryKeys, Options).

%% @equiv get_header(?LOGI_DEFAULT_EVENT_MANAGER, Options) -> ok.
-spec get_header([get_header_option()]) -> [header_entry()].
get_header(Options) ->
    get_header(?LOGI_DEFAULT_EVENT_MANAGER, Options).

%% @doc TODO
-spec get_header(event_manager_ref(), [get_header_option()]) -> [header_entry()].
get_header(ManagerRef, Options) ->
    logi_msg_header:get_header(ManagerRef, Options).
