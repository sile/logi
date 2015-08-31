%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Logger Interface
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

%%----------------------------------------------------------
%% Channel
%%----------------------------------------------------------
-export([start_channel/1, ensure_channel_started/1]). % TODO: move to logi_channel
-export([stop_channel/1]).
-export([which_channels/0]).

%%----------------------------------------------------------
%% Appender
%%----------------------------------------------------------
-export([register_appender/2, register_appender/3]). % TODO: move to logi_channel
-export([deregister_appender/2]).
-export([find_appender/2]).
-export([which_appenders/1]).
-export([set_condition/3]).

%%----------------------------------------------------------
%% Logger
%%---------------------------------------------------------
-export([new/1]).
-export([to_map/1]).
-export([save_as_default/1, save/2]).
-export([load/1]). % load_or_new
-export([erase/1]).
-export([which_loggers/0]).
-export([set_headers/1, set_headers/2]).
-export([set_metadata/1, set_metadata/2]).
-export([delete_headers/1, delete_headers/2]).
-export([delete_metadata/1, delete_metadata/2]).

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
-export([log/4]).

%% -export([debug/1, debug/2, debug/3]).
%% -export([verbose/1, verbose/2, verbose/3]).
%% -export([info/1, info/2, info/3]).
%% -export([notice/1, notice/2, notice/3]).
%% -export([warning/1, warning/2, warning/3]).
%% -export([error/1, error/2, error/3]).
%% -export([critical/1, critical/2, critical/3]).
%% -export([alert/1, alert/2, alert/3]).
%% -export([emergency/1, emergency/2, emergency/3]).

%%----------------------------------------------------------
%% Types
%%----------------------------------------------------------
-export_type([log_level/0, severity/0]).
-export_type([channel_id/0]).
-export_type([logger/0, logger_id/0, logger_instance/0]).
-export_type([key/0, headers/0, metadata/0]).
-export_type([context_handler/0]).
-export_type([frequency_controller/0, frequency_spec/0]).
-export_type([log_options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency.
-type severity()  :: log_level().

-type channel_id() :: atom().

-type logger()            :: logger_id() | logger_instance().
-type logger_id()         :: atom().
-opaque logger_instance() :: logi_client:client().

-type key() :: atom().
-type headers() :: maps:map(key(), term()).
-type metadata() :: maps:map(key(), term()).

-type context_handler() :: {module(), term()}. % TODO:

-opaque frequency_controller() :: logi_frequency_controller:controller().
-type frequency_spec() :: todo.

-type log_options() ::
        #{
           logger    => logger(),
           location  => logi_location:location(),
           headers   => headers(),
           metadata  => metadata(),
           frequency => frequency_spec()
         }.

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
%% @doc TODO
-spec new(Options) -> logger_instance() when
      Options :: #{
        channel_id           => channel_id(),
        headers              => headers(),
        metadata             => metadata(),
        context_handler      => context_handler(),
        frequency_controller => frequency_controller()
       }.
new(Options) ->
    logi_client:make(maps:get(channel_id, Options, default_logger()), Options).

%% @doc TODO
-spec to_map(logger()) -> Map when
      Map :: #{
        channel_id           => channel_id(),
        headers              => headers(),
        metadata             => metadata(),
        context_handler      => context_handler(),
        frequency_controller => frequency_controller()
       }.
to_map(LoggerId) when is_atom(LoggerId) ->
    to_map(load(LoggerId));
to_map(LoggerInstance) ->
    logi_client:to_map(LoggerInstance).

%% @equiv save(default_logger(), LoggerInstance)
-spec save_as_default(logger_instance()) -> ok.
save_as_default(LoggerInstance) -> save(default_logger(), LoggerInstance).

-spec save(logger_id(), logger_instance()) -> ok.
save(LoggerId, LoggerInstance) ->
    erlang:error(unimplemented, [LoggerId, LoggerInstance]).

-spec load(logger_id()) -> logger_instance().
load(LoggerId) ->
    erlang:error(unimplemented, [LoggerId]).

-spec erase(logger_id()) -> undefined | logger_instance().
erase(LoggerId) ->
    erlang:error(unimplemented, [LoggerId]).

-spec which_loggers() -> [logger_id()].
which_loggers() ->
    erlang:error(unimplemented, []).

%% @equiv set_headers(Headers, default_logger())
-spec set_headers(headers()) -> logger_instance().
set_headers(Headers) -> set_headers(Headers, default_logger()).

-spec set_headers(headers(), logger()) -> logger_instance().
set_headers(Headers, Logger) ->
   erlang:error(unimplemented, [Headers, Logger]).

%% @equiv set_metadata(Metadata, default_logger())
-spec set_metadata(metadata()) -> logger_instance().
set_metadata(Metadata) -> set_metadata(Metadata, default_logger()).

-spec set_metadata(metadata(), logger()) -> logger_instance().
set_metadata(Metadata, Logger) ->
   erlang:error(unimplemented, [Metadata, Logger]).

%% @equiv delete_headers(Keys, default_logger())
-spec delete_headers([key()]) -> logger_instance().
delete_headers(Keys) -> delete_headers(Keys, default_logger()).

-spec delete_headers([key()], logger()) -> logger_instance().
delete_headers(Keys, Logger) ->
    erlang:error(unimplemented, [Keys, Logger]).

%% @equiv delete_metadata(Keys, default_logger())
-spec delete_metadata([key()]) -> logger_instance().
delete_metadata(Keys) -> delete_metadata(Keys, default_logger()).

-spec delete_metadata([key()], logger()) -> logger_instance().
delete_metadata(Keys, Logger) ->
    erlang:error(unimplemented, [Keys, Logger]).

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
-spec log(severity(), io:format(), [term()], log_options()) -> logger_instance().
log(Severity, Format, FormatArgs, Options) ->
    erlang:erlang(unimplemented, [Severity, Format, FormatArgs, Options]).

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
