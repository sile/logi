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
%% Channel
%%----------------------------------------------------------
-export([start_channel/1, ensure_channel_started/1]).
-export([stop_channel/1]).
-export([which_channels/0]).

%%----------------------------------------------------------
%% Appender
%%----------------------------------------------------------
-export([register_appender/2, register_appender/3]).
-export([deregister_appender/2]).
-export([find_appender/2]).
-export([which_appenders/1]).
-export([set_condition/3]).

%%----------------------------------------------------------
%% Logger
%%---------------------------------------------------------
-export([new/1, new/2]).
-export([to_map/1, from_map/1]).
-export([save/1, save/2]).
-export([load/0, load/1]).
%% -export([set_headers/1, set_headers/2]).
%% -export([set_metadata/1, set_metadata/2]).

%%----------------------------------------------------------
%% Logging
%%---------------------------------------------------------

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
-export_type([log_level/0, severity/0]).
-export_type([channel_id/0]).
-export_type([logger/0, logger_id/0, logger_instance/0, save_id/0]).
-export_type([key/0, headers/0, metadata/0]).
-export_type([context_handler/0]).
-export_type([frequency_controller/0]).

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

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency.
-type severity()  :: log_level().

-type channel_id() :: atom().

-type logger_id()         :: atom().
-type logger()            :: save_id() | logger_instance().
-type save_id()           :: atom().
-opaque logger_instance() :: logi_client:client().

-type key() :: atom().
-type headers() :: maps:map(key(), term()).
-type metadata() :: maps:map(key(), term()).

-type context_handler() :: {module(), term()}. % TODO:

-opaque frequency_controller() :: logi_frequency_controller:controller().

%% -type context()     :: logi_context:context(). % opaqueにしたい
%% -type context_id()  :: atom().
%% -type context_ref() :: context() | context_id().
%% -type client_ref() :: context_ref().

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

%% %%----------------------------------------------------------------------------------------------------------------------
%% %% Macros
%% %%----------------------------------------------------------------------------------------------------------------------
%% -define(CONTEXT_TAG, '__LOGI_CONTEXT__').

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
%%----------------------------------------------------------
%% Constants
%%----------------------------------------------------------
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

%%----------------------------------------------------------
%% Logger
%%----------------------------------------------------------
%% @doc Starts a channel
%%
%% If a channel with the same Id already exists, it will return `{error, {already_started, pid()}}'.
-spec start_channel(channel_id()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_channel(ChannelId) when is_atom(ChannelId) ->
    case logi_channel_sup:start_child(ChannelId) of
        {ok, Pid}                       -> {ok, Pid};
        {error, {already_started, Pid}} -> {error, {already_started, Pid}};
        Other                           -> erlang:error({unexpected_result, Other}, [ChannelId])
    end;
start_channel(ChannelId) -> erlang:error(badarg, [ChannelId]).

%% @doc Equivalent to {@link start_channel/1} except it returns `{ok, pid()}' for already started channels
-spec ensure_channel_started(channel_id()) -> {ok, pid()}.
ensure_channel_started(ChannelId) ->
    case start_channel(ChannelId) of
        {ok, Pid}                       -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%% @doc Stops a channel
%%
%% If the channel `ChannelId' have not been started, it will be silently ignored.
-spec stop_channel(channel_id()) -> ok.
stop_channel(ChannelId) when is_atom(ChannelId) -> logi_channel_sup:stop_child(ChannelId);
stop_channel(ChannelId)                         -> erlang:error(badarg, [ChannelId]).

%% @doc Returns a list of all running channels
-spec which_channels() -> [channel_id()].
which_channels() -> logi_channel_sup:which_children().

%% @doc TODO
-spec set_condition(channel_id(), logi_appender:id(), logi_appender:condition()) -> {ok, logi_appender:condition()} | error.
set_condition(ChannelId, AppenderId, Condition) ->
    logi_channel:set_condition(ChannelId, AppenderId, Condition).

%%----------------------------------------------------------
%% Appender
%%----------------------------------------------------------
%% @equiv register_appender(LoggerId, Appender, #{})
-spec register_appender(logger_id(), logi_appender:appender()) -> {ok, undefined} | {error, Reason} when
      Reason :: {already_registered, logi_appender:appender()}.
register_appender(LoggerId, Appender) ->
    register_appender(LoggerId, Appender, #{}).

%% @doc Registers an appender
%%
%% TODO: doc
-spec register_appender(logger_id(), logi_appender:appender(), Options) -> {ok, OldAppender} | {error, Reason} when
      Options :: #{
        lifetime   => timeout() | pid(),
        if_exists  => error | ignore | supersede
       },
      OldAppender :: undefined | logi_appender:appender(),
      Reason :: {already_registered, logi_appender:appender()}.
register_appender(LoggerId, Appender, Options) ->
    logi_channel:register_appender(LoggerId, Appender, Options).

%% @doc Deregisters an appender
-spec deregister_appender(logger_id(), logi_appender:id()) -> {ok, logi_appender:appender()} | error.
deregister_appender(LoggerId, AppenderId) ->
    logi_channel:deregister_appender(LoggerId, AppenderId).

%% @doc TODO
-spec find_appender(logger_id(), logi_appender:id()) -> {ok, logi_appender:appender()} | error.
find_appender(LoggerId, AppenderId) ->
    logi_channel:find_appender(LoggerId, AppenderId).

%% @doc Returns a list of registered appenders
-spec which_appenders(logger_id()) -> [logi_appender:id()].
which_appenders(LoggerId) ->
    logi_channel:which_appenders(LoggerId).

%%----------------------------------------------------------
%% Logger Instance
%%----------------------------------------------------------
%% @equiv new(LoggerId, #{})
-spec new(logger_id()) -> logger_instance().
new(LoggerId) -> new(LoggerId, #{}).

%% @doc TODO
-spec new(logger_id(), Options) -> logger_instance() when
      Options :: #{
        headers               => headers(),
        metadata             => metadata(),
        context_handler      => context_handler(),
        frequency_controller => frequency_controller()
       }.
new(LoggerId, Options) ->
    logi_client:make(LoggerId, Options).

%% @doc TODO
-spec to_map(logger_instance()) -> Map when
      Map :: #{
        logger_id            => logger_id(),
        headers              => headers(),
        metadata             => metadata(),
        context_handler      => context_handler(),
        frequency_controller => frequency_controller()
       }.
to_map(LoggerInstance) ->
    logi_client:to_map(LoggerInstance).

-spec from_map(Map) -> logger_instance() when
      Map :: #{
        logger_id            => logger_id(),
        headers              => headers(),
        metadata             => metadata(),
        context_handler      => context_handler(),
        frequency_controller => frequency_controller()
       }.
from_map(Map) ->
    erlang:error(unimplemented, [Map]).

%% @equiv save(default_logger(), LoggerInstance)
-spec save(logger_instance()) -> ok. % TODO: save_as_default
save(LoggerInstance) -> save(default_logger(), LoggerInstance).

-spec save(save_id(), logger_instance()) -> ok.
save(SaveId, LoggerInstance) ->
    erlang:error(unimplemented, [SaveId, LoggerInstance]).

%% @equiv load(default_logger())
-spec load() -> logger_instance().
load() -> load(default_logger()).

-spec load(save_id()) -> logger_instance().
load(SaveId) ->
    erlang:error(unimplemented, [SaveId]).

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
