%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A Logger Interface Library
-module(logi).

-compile({no_auto_import, [error/1, error/2]}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

%%----------------------------------------------------------
%% Constants
%%----------------------------------------------------------
-export([log_levels/0]).
-export([default_logger/0]).
%% -export([default_on_expire/4]).  % TODO: default_on_expire

%%----------------------------------------------------------
%% Logger
%%----------------------------------------------------------
-export([start_logger/1, ensure_logger_started/1]).
-export([stop_logger/1]).
-export([which_loggers/0]).

%% %%----------------------------------------------------------
%% %% Backend
%% %%----------------------------------------------------------
%% -export([register_backend/3]).
%% -export([update_backend/2]).
%% -export([deregister_backend/1, deregister_backend/2]).
%% -export([find_backend/1, find_backend/2]).
%% -export([which_backends/0, which_backends/1]).

%% %%----------------------------------------------------------
%% %% Client
%% %%----------------------------------------------------------
%% -export([log/2, log/3, log/4]).
%% -export([debug/1, debug/2, debug/3]).
%% -export([verbose/1, verbose/2, verbose/3]).
%% -export([info/1, info/2, info/3]).
%% -export([notice/1, notice/2, notice/3]).
%% -export([warning/1, warning/2, warning/3]).
%% -export([error/1, error/2, error/3]).
%% -export([critical/1, critical/2, critical/3]).
%% -export([alert/1, alert/2, alert/3]).
%% -export([emergency/1, emergency/2, emergency/3]).

%% -export([make_client/1]).
%% -export([save_client/2]). % TODO: save_client/1
%% -export([load_client/1]).
%% -export([erase_client/1]).
%% -export([which_clients/0]).
%% -export([client_to_map/1]).

%% -export([set_headers/1]).
%% -export([set_metadata/1]).
%% -export([delete_headers/1]).
%% -export([delete_metadata/1]).

%% %%----------------------------------------------------------
%% %% Location
%% %%----------------------------------------------------------
%% -export([location/0, location/3, location/6]).
%% -export([location_to_map/1]).

%%----------------------------------------------------------
%% Types
%%----------------------------------------------------------
-export_type([log_level/0]).
-export_type([logger/0, logger_id/0, logger_instance/0]).
%%               severity/0,

%%               metadata/0,
%%               metadata_entry/0,
%%               metadata_entry_key/0,
%%               metadata_entry_value/0,

%%               headers/0,
%%               header/0,
%%               header_key/0,
%%               header_value/0,

%%               context/0,
%%               context_id/0,
%%               context_ref/0,

%%               log_options/0,
%%               frequency_policy/0
%%              ]).

%% %%----------------------------------------------------------------------------------------------------------------------
%% %% Types
%% %%----------------------------------------------------------------------------------------------------------------------
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency.
%% -type severity()  :: log_level().

-type logger() :: logger_id() | logger_instance().
-type logger_id() :: atom().
-type logger_instance() :: todo.

%% -type context()     :: logi_context:context(). % opaqueにしたい
%% -type context_id()  :: atom().
%% -type context_ref() :: context() | context_id().
%% -type client_ref() :: context_ref().

%% -type headers()      :: [header()].
%% -type header()       :: {header_key(), header_value()}.
%% -type header_key()   :: term().
%% -type header_value() :: term().

%% -type metadata()             :: [metadata_entry()].
%% -type metadata_entry()       :: {metadata_entry_key(), metadata_entry_value()}.
%% -type metadata_entry_key()   :: term().
%% -type metadata_entry_value() :: term().

%% -type seconds() :: non_neg_integer().

%% -type frequency_policy() :: #{intensity => non_neg_integer(),
%%                               period    => seconds(),
%%                               on_expire => function(), % XXX:
%%                               max_flush_count => pos_integer(),
%%                               id => term()}. % TODO: description

%% -type log_options() ::
%%         #{
%%            logger => client_ref(),
%%            location => logi_location:location(),
%%            headers => headers(),  % default: []
%%            metadata => metadata(), % default: []
%%            frequency => frequency_policy()
%%          }. % default: always
%% %% TODO: location (?)

%% %% TODO: export
%% -type register_backend_options() ::
%%         #{
%%            extra_arg => logi_backend:extra_arg(),
%%            condition => logi_condition:spec(),
%%            ttl       => timeout(),
%%            owner     => pid(),
%%            if_exists => error | ignore | supersede,
%%            logger    => logger()
%%          }.

%% -type update_backend_options() ::
%%         #{
%%            extra_arg => logi_backend:extra_arg(),
%%            condition => logi_condition:spec(),
%%            ttl       => timeout(),
%%            owner     => undefined | pid(),
%%            logger    => logger()
%%          }.

%% -type deregister_backend_options() :: #{logger => logger()}.

%% -type find_backend_options() :: #{logger => logger()}.

%% -type which_backends_options() :: #{logger => logger()}.

%% -type backend_info() ::
%%         #{
%%            id        => logi_backend:id(),
%%            module    => logi_backend:callback_module(),
%%            extra_arg => logi_backend:extra_arg(),
%%            condition => logi_condition:spec(),
%%            ttl       => timeout(),
%%            owner     => pid()
%%          }.

%% %%----------------------------------------------------------------------------------------------------------------------
%% %% Macros
%% %%----------------------------------------------------------------------------------------------------------------------
%% -define(CONTEXT_TAG, '__LOGI_CONTEXT__').

%% %% TODO: move to logi_logger module
%% -define(LOGGER_RUNNING_CHECK(LoggerId),
%%         _ = case whereis(LoggerId) of
%%                 undefined -> erlang:error({logger_is_not_running, LoggerId});
%%                 _         -> ok
%%             end).

%% -define(WITH_CONTEXT(ContextRef, Fun),
%%         case is_atom(ContextRef) of
%%             false -> (Fun)(ContextRef);
%%             true  -> ok = save_context(ContextRef, (Fun)(load_context(ContextRef))), ContextRef
%%         end).

%% -define(WITH_READ_CONTEXT(ContextRef, Fun),
%%         case logi_context:is_context(ContextRef) of
%%             true  -> (Fun)(ContextRef);
%%             false -> (Fun)(load_context(ContextRef))
%%         end).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
%% @doc Returns the default logger
%%
%% The default logger is started automatically when `logi' application was started.
-spec default_logger() -> logger_id().
default_logger() -> logi_default_logger.

%% @doc Returns the available log level list
%%
%% The log levels are ordered by the severity (The lowest severity level will appear first).
-spec log_levels() -> [log_level()].
log_levels() -> [debug, verbose, info, notice, warning, error, critical, alert, emergency].

%% %% TODO
%% default_on_expire(Context, Id, Count, Info) ->
%%     %% TODO: max_flush_count=0
%%     Duration = timer:now_diff(os:timestamp(), logi_msg_info:get_timestamp(Info)) / 1000 / 1000,
%%     log(logi_msg_info:get_severity(Info),
%%         "Over ~p seconds, ~p messages were dropped (id: ~p)", [Duration, Count, Id],
%%         #{logger => Context,
%%           location => logi_msg_info:get_location(Info),
%%           headers => logi_msg_info:get_headers(Info),
%%           metadata => logi_msg_info:get_metadata(Info)}).

%%------------------------------------------------------------------------------
%% Logger
%%------------------------------------------------------------------------------
%% @doc Starts a new logger
%%
%% If a logger with the same Id already exists, it will return `{error, {already_started, pid()}}'.
-spec start_logger(logger_id()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_logger(LoggerId) when is_atom(LoggerId) ->
    case logi_logger_sup:start_child(LoggerId) of
        {ok, Pid}                       -> {ok, Pid};
        {error, {already_started, Pid}} -> {error, {already_started, Pid}};
        Other                           -> erlang:error({unexpected_result, Other}, [LoggerId])
    end;
start_logger(LoggerId) -> erlang:error(badarg, [LoggerId]).

%% @doc Equivalent to {@link start_logger/1} except it returns `{ok, pid()}' for already started loggers
-spec ensure_logger_started(logger_id()) -> {ok, pid()}.
ensure_logger_started(LoggerId) ->
    case start_logger(LoggerId) of
        {ok, Pid}                       -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%% @doc Stops the logger
%%
%% If the logger `LoggerId' have not been started, it will be silently ignored.
-spec stop_logger(logger_id()) -> ok.
stop_logger(LoggerId) when is_atom(LoggerId) -> logi_logger_sup:stop_child(LoggerId);
stop_logger(LoggerId)                        -> erlang:error(badarg, [LoggerId]).

%% @doc Returns a list of all running loggers
-spec which_loggers() -> [logger_id()].
which_loggers() -> logi_logger_sup:which_children().

%% %%------------------------------------------------------------------------------
%% %% Exported Functions: Backend API
%% %%------------------------------------------------------------------------------
%% %% @doc Registers the backend
%% -spec register_backend(logi_backend:id(), logi_backend:callback_module(), register_backend_options()) ->
%%                               {ok, OldBackend} | {error, Reason} when
%%       OldBackend :: undefined | backend_info(),
%%       Reason     :: {already_registered, backend_info()}.
%% register_backend(BackendId, BackendModlue, Options) ->
%%     logi_backend_manager:register_backend(BackendId, BackendModlue, Options).

%% %% @doc Updates the backend
%% -spec update_backend(logi_backend:id(), update_backend_options()) -> {ok, OldBackend} | {error, Reason} when
%%       OldBackend :: undefined | backend_info(),
%%       Reason     :: unregistered.
%% update_backend(BackendId, Options) ->
%%     logi_backend_manager:update_backend(BackendId, Options).

%% %% @equiv deregister_backend(BackendId, #{})
%% -spec deregister_backend(logi_backend:id()) -> DeregisteredBackend when
%%       DeregisteredBackend :: undefined | backend_info().
%% deregister_backend(BackendId) ->
%%     deregister_backend(BackendId, #{}).

%% %% @doc Deregisters the backend
%% -spec deregister_backend(logi_backend:id(), deregister_backend_options()) -> DeregisteredBackend when
%%       DeregisteredBackend :: undefined | backend_info().
%% deregister_backend(BackendId, Options) ->
%%     logi_backend_manager:deregister_backend(BackendId, Options).

%% %% @equiv find_backend(BackendId, #{})
%% -spec find_backend(logi_backend:id()) -> {ok, backend_info()} | {error, not_found}.
%% find_backend(BackendId) ->
%%     find_backend(BackendId, #{}).

%% %% @doc Finds the backend
%% -spec find_backend(logi_backend:id(), find_backend_options()) -> {ok, backend_info()} | {error, not_found}.
%% find_backend(BackendId, Options) ->
%%     logi_backend_manager:find_backend(BackendId, Options).

%% %% @equiv which_backends(#{})
%% -spec which_backends() -> [logi_backend:id()].
%% which_backends() ->
%%     which_backends(#{}).

%% %% @doc Returns a list of registered backends
%% -spec which_backends(which_backends_options()) -> [logi_backend:id()].
%% which_backends(Options) ->
%%     logi_backend_manager:which_backends(Options).

%% %%------------------------------------------------------------------------------
%% %% Exported Functions: Logging API
%% %%------------------------------------------------------------------------------
%% %% @equiv log(Severity, Format, [])
%% -spec log(severity(), io:format()) -> client_ref().
%% log(Severity, Format) -> log(Severity, Format, []).

%% %% @equiv log(Severity, Format, Args, #{})
%% -spec log(severity(), io:format(), [term()]) -> client_ref().
%% log(Severity, Format, Args) -> log(Severity, Format, Args, #{}).

%% %% @doc Outputs the log message
%% %%
%% %% TODO: doc: logi_transform
%% -spec log(severity(), io:format(), [term()], log_options()) -> client_ref().
%% log(Severity, Format, Args, Options) ->
%%     ClientRef = maps:get(logger, Options, default_logger()),
%%     ?WITH_CONTEXT(ClientRef,
%%                   fun (Client0) ->
%%                           case logi_client:ready(Client0, Severity, Options) of
%%                               {skip, Client1}                  -> Client1;
%%                               {ok, Backends, MsgInfo, Client1} -> _ = logi_client:write(Backends, MsgInfo, Format, Args), Client1
%%                           end
%%                   end).

%% -spec debug(io:format()) -> context_ref().
%% debug(Format) -> debug(Format, []).

%% -spec debug(io:format(), [term()]) -> context_ref().
%% debug(Format, Args) -> debug(Format, Args, #{}).

%% -spec debug(io:format(), [term()], log_options()) -> context_ref().
%% debug(Format, Args, Options) -> log(debug, Format, Args, Options).

%% -spec verbose(io:format()) -> context_ref().
%% verbose(Format) -> verbose(Format, []).

%% -spec verbose(io:format(), [term()]) -> context_ref().
%% verbose(Format, Args) -> verbose(Format, Args, #{}).

%% -spec verbose(io:format(), [term()], log_options()) -> context_ref().
%% verbose(Format, Args, Options) -> log(verbose, Format, Args, Options).

%% -spec info(io:format()) -> context_ref().
%% info(Format) -> info(Format, []).

%% -spec info(io:format(), [term()]) -> context_ref().
%% info(Format, Args) -> info(Format, Args, #{}).

%% -spec info(io:format(), [term()], log_options()) -> context_ref().
%% info(Format, Args, Options) -> log(info, Format, Args, Options).

%% -spec notice(io:format()) -> context_ref().
%% notice(Format) -> notice(Format, []).

%% -spec notice(io:format(), [term()]) -> context_ref().
%% notice(Format, Args) -> notice(Format, Args, #{}).

%% -spec notice(io:format(), [term()], log_options()) -> context_ref().
%% notice(Format, Args, Options) -> log(notice, Format, Args, Options).

%% -spec warning(io:format()) -> context_ref().
%% warning(Format) -> warning(Format, []).

%% -spec warning(io:format(), [term()]) -> context_ref().
%% warning(Format, Args) -> warning(Format, Args, #{}).

%% -spec warning(io:format(), [term()], log_options()) -> context_ref().
%% warning(Format, Args, Options) -> log(warning, Format, Args, Options).

%% -spec error(io:format()) -> context_ref().
%% error(Format) -> error(Format, []).

%% -spec error(io:format(), [term()]) -> context_ref().
%% error(Format, Args) -> error(Format, Args, #{}).

%% -spec error(io:format(), [term()], log_options()) -> context_ref().
%% error(Format, Args, Options) -> log(error, Format, Args, Options).

%% -spec critical(io:format()) -> context_ref().
%% critical(Format) -> critical(Format, []).

%% -spec critical(io:format(), [term()]) -> context_ref().
%% critical(Format, Args) -> critical(Format, Args, #{}).

%% -spec critical(io:format(), [term()], log_options()) -> context_ref().
%% critical(Format, Args, Options) -> log(critical, Format, Args, Options).

%% -spec alert(io:format()) -> context_ref().
%% alert(Format) -> alert(Format, []).

%% -spec alert(io:format(), [term()]) -> context_ref().
%% alert(Format, Args) -> alert(Format, Args, #{}).

%% -spec alert(io:format(), [term()], log_options()) -> context_ref().
%% alert(Format, Args, Options) -> log(alert, Format, Args, Options).

%% %% TODO: obsolute annotation (?)
%% -spec emergency(io:format()) -> context_ref().
%% emergency(Format) -> emergency(Format, []).

%% -spec emergency(io:format(), [term()]) -> context_ref().
%% emergency(Format, Args) -> emergency(Format, Args, #{}).

%% -spec emergency(io:format(), [term()], log_options()) -> context_ref(). % TODO: -> client() (?)
%% emergency(Format, Args, Options) -> log(emergency, Format, Args, Options).

%% %%------------------------------------------------------------------------------
%% %% Exported Functions: Client
%% %%------------------------------------------------------------------------------
%% %% @doc Makes a new logger client instance
%% -spec make_client(Options) -> client() when
%%       Options :: #{
%%         logger   => logger(),
%%         headers  => headers(),
%%         metadata => metadata()
%%        }.
%% make_client(Options) -> logi_client:make(Options).

%% %% @doc Saves the client in the process dictionary
%% -spec save_client(client_id(), client()) -> ok.
%% save_client(Id, Client) ->
%%     case is_atom(Id) andalso logi_client:is_client(Client) of
%%         false -> erlang:error(badarg, [Client, Id]);
%%         true  -> _ = put({?CLIENT_TAG, Id}, Client), ok
%%     end.

%% %% @doc Loads the client from the process dictionary
%% %%
%% %% If the client does not exist, the result of `make_client(#{logger => Id})' will return.
%% -spec load_client(client_id()) -> client().
%% load_client(Id) when is_atom(Id) ->
%%     case get({?CLIENT_TAG, Id}) of
%%         undefined -> make_client(#{logger => Id});
%%         Client    -> Client
%%     end;
%% load_client(Id) -> erlang:error(badarg, [Id]).

%% %% @doc Erases the client from the process dictionary
%% -spec erase_client(client_id()) -> undefined | client().
%% erase_client(Id) when is_atom(Id) -> erase({?CLIENT_TAG, Id});
%% erase_client(Id)                  -> error(badarg, [Id]).

%% %% @doc Returns a list of saved clients
%% -spec which_clients() -> [client_id()].
%% which_clients() ->
%%     [Id || {{?CLIENT_TAG, Id}, _} <- get()].

%% %%------------------------------------------------------------------------------
%% %% Exported Functions: Headers
%% %%------------------------------------------------------------------------------
%% %% @doc Sets the client TODO
%% -spec set(Options) -> client_instance() when
%%       Options :: #{
%%         logger   => client(),
%%         headers  => headers(),
%%         metadata => metadata()
%%         %% TODO: severity_mapper, etc
%%        }.
%% set(_Options) ->
%%     todo.

%% %% @doc Gets the client as a map
%% -spec get(Options) -> Map when
%%       Options :: #{logger => client()},
%%       Map :: #{headers => headers(),
%%                metadata => metadata()}.
%% get(_Options) ->
%%     todo.

%% %% @doc Deletes ...
%% -spec delete(Options) -> client_instance() when
%%       Options :: #{
%%         logger => client(),
%%         headers => [atom()],
%%         metadata => [atom()]
%%        }.
%% delete(_Options) ->

%% location(_) ->
%%     todo.

%% logi:client_to_map(
%% logi:set(#{headers => #{sid => SessionId}}).
%% logi:set_headers(#{sid => SessionId}).
