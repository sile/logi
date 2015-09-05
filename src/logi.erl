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
%% Logger
%%---------------------------------------------------------
-export([new/1, new/2]).
-export([is_logger/1]).
-export([to_map/1, from_map/1]).
-export([save/2, save_as_default/1]).
-export([load/1, load_or_new/1, load_or_new/2, load_or_new/3]).
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
-export_type([logger/0, logger_id/0, logger_instance/0]).
-export_type([logger_map/0]).
-export_type([new_option/0, new_options/0]).

-export_type([headers/0, metadata/0]).
-export_type([context_handler/0]).
-export_type([frequency_controller/0, frequency_spec/0]).
-export_type([log_option/0, log_options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency. % TODO: => severity_level/0 (?)
-type severity()  :: log_level().

-type logger()            :: logger_id() | logger_instance().
-type logger_id()         :: atom().
-opaque logger_instance() :: logi_logger:logger().

-type logger_map() ::
        #{
           channel_id           => logi_channel:id(), % mandatory
           headers              => headers(), % optional
           metadata             => metadata(), % optional
           context_handler      => context_handler(), % optional
           frequency_controller => frequency_controller() % optional
         }.

-type new_options() :: [new_option()].
-type new_option() :: {headers, headers()}
                    | {metadata, metadata()}
                    | {context_handler, context_handler()}
                    | {frequency_controller, frequency_controller()}.

-type headers() :: #{}.
-type metadata() :: #{}.

-type context_handler() :: {module(), term()}. % TODO:

-type frequency_controller() :: term().
-type frequency_spec() :: todo.

-type log_options() :: [log_option()].
-type log_option() :: {logger, logger()}
                    | {location, logi_location:location()}
                    | {headers, headers()}
                    | {metadata, metadata()}
                    | {frequency, frequency_spec()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(PD_LOGGER_KEY(LoggerId), {'_LOGI_', LoggerId}).

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
default_logger() -> logi_default_log.

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
-spec new(logi_channel:id()) -> logger_instance().
new(ChannelId) -> new(ChannelId, []).

-spec new(logi_channel:id(), new_options()) -> logger_instance().
new(ChannelId, Options) -> logi_logger:new(ChannelId, Options).

-spec is_logger(logger() | term()) -> boolean().
is_logger(X) -> is_atom(X) orelse logi_logger:is_logger(X).

-spec to_map(logger()) -> logger_map().
to_map(Logger) when is_atom(Logger) -> to_map(load_or_new(Logger));
to_map(Logger)                      -> logi_logger:to_map(Logger).

-spec from_map(logger_map()) -> logger_instance().
from_map(Map) -> logi_logger:from_map(Map).

-spec save_as_default(logger()) -> undefined | logger_instance().
save_as_default(Logger) -> save(default_logger(), Logger).

-spec save(logger_id(), logger()) -> undefined | logger_instance(). % TODO: ok | error (?)
save(LoggerId, Logger) when is_atom(Logger) ->
    save(LoggerId, load_or_new(Logger));
save(LoggerId, Logger) ->
    _ = logi_logger:is_logger(Logger) orelse erlang:error(badarg, [LoggerId, Logger]),
    put(?PD_LOGGER_KEY(LoggerId), Logger).

-spec load(logger_id()) -> {ok, logger_instance()} | error.
load(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    case get(?PD_LOGGER_KEY(LoggerId)) of
        undefined -> error;
        Logger    -> {ok, Logger}
    end.

-spec load_or_new(logger_id()) -> logger_instance().
load_or_new(LoggerId) -> load_or_new(LoggerId, LoggerId).

-spec load_or_new(logger_id(), logi_channel:id()) -> logger_instance().
load_or_new(LoggerId, ChannelId) -> load_or_new(LoggerId, ChannelId, []).

-spec load_or_new(logger_id(), logi_channel:id(), new_options()) -> logger_instance().
load_or_new(LoggerId, ChannelId, Options) ->
    case load(LoggerId) of
        {ok, Logger} -> Logger;
        error        -> new(ChannelId, Options)
    end.

-spec erase(logger_id()) -> undefined | logger_instance(). % TODO: ok | error (?)
erase(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    erlang:erase(?PD_LOGGER_KEY(LoggerId)).

-spec which_loggers() -> [logger_id()].
which_loggers() ->
    [LoggerId || ?PD_LOGGER_KEY(LoggerId) <- get()].

-spec set_headers(headers()) -> logger_instance().
set_headers(Headers) -> set_headers(Headers, []).

-spec set_headers(headers(), Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}.
set_headers(Headers, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Headers, Options]),
    Logger = load_or_new(proplists:get_value(logger, Options, default_logger())),
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    logi_logger:set_headers(Headers, IfExists, Logger).

-spec set_metadata(metadata()) -> logger_instance().
set_metadata(Metadata) -> set_metadata(Metadata, []).

-spec set_metadata(metadata(), Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}.
set_metadata(Metadata, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Metadata, Options]),
    Logger = load_or_new(proplists:get_value(logger, Options, default_logger())),
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    logi_logger:set_metadata(Metadata, IfExists, Logger).

-spec delete_headers([term()]) -> logger_instance().
delete_headers(Keys) -> delete_headers(Keys, []).

-spec delete_headers([term()], Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}.
delete_headers(Keys, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    Logger = load_or_new(proplists:get_value(logger, Options, default_logger())),
    logi_logger:delete_headers(Keys, Logger).

-spec delete_metadata([term()]) -> logger_instance().
delete_metadata(Keys) -> delete_metadata(Keys, []).

-spec delete_metadata([term()], Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}.
delete_metadata(Keys, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    Logger = load_or_new(proplists:get_value(logger, Options, default_logger())),
    logi_logger:delete_metadata(Keys, Logger).

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
-spec log(severity(), io:format(), [term()], log_options()) -> no_return(). %logger_instance().
log(Severity, Format, FormatArgs, Options) ->
    erlang:error(unimplemented, [Severity, Format, FormatArgs, Options]).

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
