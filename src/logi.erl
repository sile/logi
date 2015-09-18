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

%% TODO: refactoring
-export([severities/0]).
-export([severity_level/1]).
-export([is_severity/1]).

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
%% TODO: reset
%% TODO: set_filter

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
-export([log/4]).
-export([debug/1, debug/2, debug/3]).
-export([verbose/1, verbose/2, verbose/3]).
-export([info/1, info/2, info/3]).
-export([notice/1, notice/2, notice/3]).
-export([warning/1, warning/2, warning/3]).
-export([error/1, error/2, error/3]).
-export([critical/1, critical/2, critical/3]).
-export([alert/1, alert/2, alert/3]).
-export([emergency/1, emergency/2, emergency/3]).

%%----------------------------------------------------------
%% Types
%%----------------------------------------------------------
-export_type([log_level/0, severity/0]).
-export_type([logger/0, logger_id/0, logger_instance/0]).
-export_type([logger_map/0]).
-export_type([new_option/0, new_options/0]).

-export_type([headers/0, metadata/0]).
-export_type([log_option/0, log_options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency. % TODO: => severity_level/0 (?)
-type severity()  :: log_level().

-type logger() :: logger_id()
                | [logger_instance()].

-type logger_id() :: atom().
-opaque logger_instance() :: logi_logger:logger().

-type logger_map() ::
        #{
           channel_id => logi_channel:id(), % mandatory
           headers    => headers(), % optional
           metadata   => metadata(), % optional
           filters    => [logi_filter:filter()] % optional
         }.

-type new_options() :: [new_option()].
-type new_option() :: {headers, headers()}
                    | {metadata, metadata()}
                    | {filters, [logi_filter:filter()]}.

-type headers() :: #{}.
-type metadata() :: #{}.

-type log_options() :: [log_option()].
-type log_option() :: {logger, logger()}
                    | {location, logi_location:location()}
                    | {headers, headers()}
                    | {metadata, metadata()}
                    | logi_filter:option().

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(PD_LOGGER_KEY(LoggerId), {'_LOGI_', LoggerId}).

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

%% TODO: refactoring
-spec severities() -> [severity()].
severities() -> [emergency, alert, critical, error, warning, notice, info, verbose, debug].

-spec severity_level(severity()) -> 1..9. %%severity_level().
severity_level(emergency) -> 1;
severity_level(alert)     -> 2;
severity_level(critical)  -> 3;
severity_level(error)     -> 4;
severity_level(warning)   -> 5;
severity_level(notice)    -> 6;
severity_level(info)      -> 7;
severity_level(verbose)   -> 8;
severity_level(debug)     -> 9;
severity_level(Severity)  -> error(badarg, [Severity]).

%% @doc Returns `true' if `X' is a severity, otherwise `false'
-spec is_severity(X :: (severity() | term())) -> boolean().
is_severity(X) -> lists:member(X, severities()).

%%----------------------------------------------------------
%% Logger
%%----------------------------------------------------------
-spec new(logi_channel:id()) -> logger_instance().
new(ChannelId) -> new(ChannelId, []).

-spec new(logi_channel:id(), new_options()) -> logger_instance().
new(ChannelId, Options) -> logi_logger:new(ChannelId, Options).

-spec to_map(logger_instance()) -> logger_map().
to_map(Logger) -> logi_logger:to_map(Logger).

-spec from_map(logger_map()) -> logger_instance().
from_map(Map) -> logi_logger:from_map(Map).

-spec is_logger(logger() | term()) -> boolean().
is_logger(X) when is_atom(X) -> true;
is_logger(X) when is_list(X) -> lists:all(fun logi_logger:is_logger/1, X);
is_logger(_)                 -> false.

-spec save_as_default(logger()) -> Old::[logger_instance()].
save_as_default(Logger) -> save(default_logger(), Logger).

-spec save(logger_id(), logger()) -> Old::[logger_instance()].
save(LoggerId, Logger) when is_atom(Logger) ->
    save(LoggerId, load_or_new(Logger));
save(LoggerId, []) ->
    logi:erase(LoggerId);
save(LoggerId, Logger) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId, Logger]),
    _ = is_list(Logger) andalso lists:all(fun logi_logger:is_logger/1, Logger) orelse erlang:error(badarg, [LoggerId, Logger]),
    case put(?PD_LOGGER_KEY(LoggerId), Logger) of
        undefined -> [];
        Old       -> Old
    end.

-spec load(logger_id()) -> [logger_instance()].
load(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    case get(?PD_LOGGER_KEY(LoggerId)) of
        undefined -> [];
        Logger    -> Logger
    end.

-spec load_or_new(logger_id()) -> [logger_instance()].
load_or_new(LoggerId) -> load_or_new(LoggerId, LoggerId).

-spec load_or_new(logger_id(), logi_channel:id()) -> [logger_instance()].
load_or_new(LoggerId, ChannelId) -> load_or_new(LoggerId, ChannelId, []).

-spec load_or_new(logger_id(), logi_channel:id(), new_options()) -> [logger_instance()].
load_or_new(LoggerId, ChannelId, Options) ->
    case load(LoggerId) of
        []     -> [new(ChannelId, Options)];
        Logger -> Logger
    end.

-spec erase(logger_id()) -> Old :: [logger_instance()].
erase(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    case erlang:erase(?PD_LOGGER_KEY(LoggerId)) of
        undefined -> [];
        Logger    -> Logger
    end.

-spec which_loggers() -> [logger_id()].
which_loggers() ->
    [LoggerId || {?PD_LOGGER_KEY(LoggerId), _} <- get()].

-spec set_headers(headers()) -> [logger_instance()].
set_headers(Headers) -> set_headers(Headers, []).

-spec set_headers(headers(), Options) -> [logger_instance()] when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}.
set_headers(Headers, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Headers, Options]), % TODO: redundant error handling
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = [logi_logger:set_headers(Headers, IfExists, Instance) || Instance <- Logger0],
    save_if_need(Need, Logger1).

-spec set_metadata(metadata()) -> [logger_instance()].
set_metadata(Metadata) -> set_metadata(Metadata, []).

-spec set_metadata(metadata(), Options) -> [logger_instance()] when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}.
set_metadata(Metadata, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Metadata, Options]), % TODO: redundant error handling
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = [logi_logger:set_metadata(Metadata, IfExists, Instance) || Instance <- Logger0],
    save_if_need(Need, Logger1).

-spec delete_headers([term()]) -> [logger_instance()].
delete_headers(Keys) -> delete_headers(Keys, []).

-spec delete_headers([term()], Options) -> [logger_instance()] when
      Options :: [Option],
      Option  :: {logger, logger()}.
delete_headers(Keys, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = [logi_logger:delete_headers(Keys, Instance) || Instance <- Logger0],
    save_if_need(Need, Logger1).

-spec delete_metadata([term()]) -> [logger_instance()].
delete_metadata(Keys) -> delete_metadata(Keys, []).

-spec delete_metadata([term()], Options) -> [logger_instance()] when
      Options :: [Option],
      Option  :: {logger, logger()}.
delete_metadata(Keys, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = [logi_logger:delete_metadata(Keys, Instance) || Instance <- Logger0],
    save_if_need(Need, Logger1).

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
%% @deprecated TODO
%%
%% TODO: private(?)
-spec log(severity(), io:format(), [term()], log_options()) -> [logger_instance()].
log(Severity, Format, FormatArgs, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Severity, Format, FormatArgs, Options]),
    DefaultLocation =
        case lists:keyfind(location, 1, Options) of
            false         -> logi_location:guess_location();
            {_, Location} -> Location
        end,
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 =
        [case logi_logger:ready(Instance0, Severity, DefaultLocation, Options) of
             {Sinks, Context, Instance1} ->
                 ok = logi_logger:write(Sinks, Context, Format, FormatArgs),
                 Instance1;
             Instance1 ->
                 Instance1
         end || Instance0 <- Logger0],
    save_if_need(Need, Logger1).

-spec debug(io:format()) -> [logger_instance()].
debug(Format) -> debug(Format, []).

-spec debug(io:format(), [term()]) -> [logger_instance()].
debug(Format, Args) -> debug(Format, Args, []).

-spec debug(io:format(), [term()], log_options()) -> [logger_instance()].
debug(Format, Args, Options) -> log(debug, Format, Args, Options).

-spec verbose(io:format()) -> [logger_instance()].
verbose(Format) -> verbose(Format, []).

-spec verbose(io:format(), [term()]) -> [logger_instance()].
verbose(Format, Args) -> verbose(Format, Args, []).

-spec verbose(io:format(), [term()], log_options()) -> [logger_instance()].
verbose(Format, Args, Options) -> log(verbose, Format, Args, Options).

-spec info(io:format()) -> [logger_instance()].
info(Format) -> info(Format, []).

-spec info(io:format(), [term()]) -> [logger_instance()].
info(Format, Args) -> info(Format, Args, []).

-spec info(io:format(), [term()], log_options()) -> [logger_instance()].
info(Format, Args, Options) -> log(info, Format, Args, Options).

-spec notice(io:format()) -> [logger_instance()].
notice(Format) -> notice(Format, []).

-spec notice(io:format(), [term()]) -> [logger_instance()].
notice(Format, Args) -> notice(Format, Args, []).

-spec notice(io:format(), [term()], log_options()) -> [logger_instance()].
notice(Format, Args, Options) -> log(notice, Format, Args, Options).

-spec warning(io:format()) -> [logger_instance()].
warning(Format) -> warning(Format, []).

-spec warning(io:format(), [term()]) -> [logger_instance()].
warning(Format, Args) -> warning(Format, Args, []).

-spec warning(io:format(), [term()], log_options()) -> [logger_instance()].
warning(Format, Args, Options) -> log(warning, Format, Args, Options).

-spec error(io:format()) -> [logger_instance()].
error(Format) -> error(Format, []).

-spec error(io:format(), [term()]) -> [logger_instance()].
error(Format, Args) -> error(Format, Args, []).

-spec error(io:format(), [term()], log_options()) -> [logger_instance()].
error(Format, Args, Options) -> log(error, Format, Args, Options).

-spec critical(io:format()) -> [logger_instance()].
critical(Format) -> critical(Format, []).

-spec critical(io:format(), [term()]) -> [logger_instance()].
critical(Format, Args) -> critical(Format, Args, []).

-spec critical(io:format(), [term()], log_options()) -> [logger_instance()].
critical(Format, Args, Options) -> log(critical, Format, Args, Options).

-spec alert(io:format()) -> [logger_instance()].
alert(Format) -> alert(Format, []).

-spec alert(io:format(), [term()]) -> [logger_instance()].
alert(Format, Args) -> alert(Format, Args, []).

-spec alert(io:format(), [term()], log_options()) -> [logger_instance()].
alert(Format, Args, Options) -> log(alert, Format, Args, Options).

-spec emergency(io:format()) -> [logger_instance()].
emergency(Format) -> emergency(Format, []).

-spec emergency(io:format(), [term()]) -> [logger_instance()].
emergency(Format, Args) -> emergency(Format, Args, []).

-spec emergency(io:format(), [term()], log_options()) -> [logger_instance()].
emergency(Format, Args, Options) -> log(emergency, Format, Args, Options).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec save_if_need({ok, logger_id()} | error, [logger_instance()]) -> [logger_instance()].
save_if_need(error,    Logger) -> Logger;
save_if_need({ok, Id}, Logger) -> _ = save(Id, Logger), Logger.

-spec load_if_need(logger()) -> {{ok, logger_id()} | error, [logger_instance()]}.
load_if_need(Logger) when is_atom(Logger) -> {{ok, Logger}, load_or_new(Logger)};
load_if_need(Logger)                      -> {error, Logger}.
