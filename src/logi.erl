%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Logger Interface
%%
%% TODO: doc
%%
%% TODO: examples
%%
-module(logi).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
%%----------------------------------------------------------
%% Constants
%%----------------------------------------------------------
-export([default_logger/0]).
-export([severities/0]).
-export([severity_level/1]).
-export([is_severity/1]).

%%----------------------------------------------------------
%% Logger
%%---------------------------------------------------------
-export([new/0, new/1, new/2]).
-export([is_logger/1]).
-export([to_map/1, from_map/1]).
-export([to_list/1, from_list/1]).
-export([save/2, save_as_default/1]).
-export([load/1, load_or_new/1, load_or_new/2, load_or_new/3]).
-export([erase/0, erase/1]).
-export([which_loggers/0]).

-export([set_headers/1, set_headers/2]).
-export([set_metadata/1, set_metadata/2]).
-export([delete_headers/1, delete_headers/2]).
-export([delete_metadata/1, delete_metadata/2]).

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
-export_type([logger_map_form/0]).
-export_type([new_option/0, new_options/0]).

-export_type([headers/0, metadata/0]).
-export_type([log_option/0, log_options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type log_level() :: debug | verbose | info | notice | warning | error | critical | alert | emergency. % TODO: => severity_level/0 (?)
-type severity()  :: log_level().

-type logger() :: logger_id() | logger_instance().

-type logger_id() :: atom().
-opaque logger_instance() :: logi_logger:logger().

-type logger_map_form() ::
        #{
           channel_id => logi_channel:id(), % mandatory
           headers    => headers(), % optional
           metadata   => metadata(), % optional
           filters    => [logi_filter:filter()] % optional
         }.

-type new_options() :: [new_option()].
-type new_option() :: {headers, headers()}
                    | {metadata, metadata()}
                    | {filter, logi_filter:filter()}
                    | {next, logger_instance()}.

-type headers() :: #{}.
-type metadata() :: #{}.

-type log_options() :: [log_option()].
-type log_option() :: {logger, logger()}
                    | {location, logi_location:location()}
                    | {subject, term()}
                    | {headers, headers()}
                    | {metadata, metadata()}
                    | {timestamp, erlang:timestamp()}.

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

%% @doc Returns the available severity list
%%
%% The list are ordered by the their severity level (see: {@link severity_level/1}).
-spec severities() -> [severity()].
severities() -> [emergency, alert, critical, error, warning, notice, info, verbose, debug].

%% @doc Returns the level of `Severity'
%%
%% The higher the severity is, the lower the level is.
-spec severity_level(Severity :: severity()) -> 1..9.
severity_level(emergency) -> 1;
severity_level(alert)     -> 2;
severity_level(critical)  -> 3;
severity_level(error)     -> 4;
severity_level(warning)   -> 5;
severity_level(notice)    -> 6;
severity_level(info)      -> 7;
severity_level(verbose)   -> 8;
severity_level(debug)     -> 9;
severity_level(Severity)  -> erlang:error(badarg, [Severity]).

%% @doc Returns `true' if `X' is a severity, otherwise `false'
-spec is_severity(X :: (severity() | term())) -> boolean().
is_severity(X) -> lists:member(X, severities()).

%%----------------------------------------------------------
%% Logger
%%----------------------------------------------------------
%% @equiv new(default_logger())
-spec new() -> logger_instance().
new() -> new(default_logger()).

%% @equiv new(ChannelId, [])
-spec new(logi_channel:id()) -> logger_instance().
new(ChannelId) -> new(ChannelId, []).

%% @doc Creates a new logger instance
-spec new(logi_channel:id(), new_options()) -> logger_instance().
new(ChannelId, Options) -> logi_logger:new(ChannelId, Options).

%% @doc Returns `true' if `X' is a logger, otherwise `false'
-spec is_logger(X :: (logger() | term())) -> boolean().
is_logger(X) -> is_atom(X) orelse logi_logger:is_logger(X).

%% @doc Converts `Logger' into a map form
%%
%% The entries which has default values will be omitted from the resulting map
-spec to_map(logger()) -> logger_map_form().
to_map(Logger) -> logi_logger:to_map(element(2, load_if_need(Logger))).

%% @doc Creates a new logger instance from `Map'
%%
%% Default Value:
%% - channel_id: none (mandatory)
%% - headers: `#{}'
%% - metadata: `#{}'
%% - filter: none (optional)
%% - next: none (optional)
-spec from_map(logger_map_form()) -> logger_instance().
from_map(Map) -> logi_logger:from_map(Map).

-spec to_list(logger()) -> [logger_instance()].
to_list(Logger) ->
    Map = to_map(Logger),
    case Map of
        #{next := Next} -> [from_map(maps:remove(next, Map)) | to_list(Next)];
        _               -> [Logger]
    end.

%% TODO: nextが既に設定されていた場合の挙動
-spec from_list([logger()]) -> logger_instance().
from_list([])      -> erlang:error(badarg, [[]]);
from_list(Loggers) ->
    lists:foldr(
      fun (Parent, undefined) -> Parent;
          (Parent, Child)     -> from_map(maps:put(next, Child, to_map(Parent)))
      end,
      undefined,
      Loggers).

%% @equiv save(default_logger(), Logger)
-spec save_as_default(logger()) -> Old::(logger_instance() | undefined).
save_as_default(Logger) -> save(default_logger(), Logger).

-spec save(logger_id(), logger()) -> Old::(logger_instance() | undefined).
save(LoggerId, Logger0) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId, Logger0]),
    _ = is_logger(Logger0) orelse erlang:error(badarg, [LoggerId, Logger0]),
    Logger1 =
        case is_atom(Logger0) of
            true  -> load_or_new(Logger0);
            false -> Logger0
        end,
    put(?PD_LOGGER_KEY(LoggerId), Logger1).

-spec load(logger_id()) -> {ok, logger_instance()} | error.
load(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    case get(?PD_LOGGER_KEY(LoggerId)) of
        undefined -> error;
        Logger    -> {ok, Logger}
    end.

%% @equiv load_or_new(LoggerId, LoggerId)
-spec load_or_new(logger_id()) -> logger_instance().
load_or_new(LoggerId) -> load_or_new(LoggerId, LoggerId).

%% @equiv load_or_new(LoggerId, ChannelId, [])
-spec load_or_new(logger_id(), logi_channel:id()) -> logger_instance().
load_or_new(LoggerId, ChannelId) -> load_or_new(LoggerId, ChannelId, []).

-spec load_or_new(logger_id(), logi_channel:id(), new_options()) -> logger_instance().
load_or_new(LoggerId, ChannelId, Options) ->
    case load(LoggerId) of
        error        -> new(ChannelId, Options);
        {ok, Logger} -> Logger
    end.

-spec erase() -> [{logger_id(), logger_instance()}].
erase() ->
    [{Id, logi:erase(Id)} || Id <- which_loggers()].

-spec erase(logger_id()) -> Old :: (logger_instance() | undefined).
erase(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    erlang:erase(?PD_LOGGER_KEY(LoggerId)).

-spec which_loggers() -> [logger_id()].
which_loggers() ->
    [LoggerId || {?PD_LOGGER_KEY(LoggerId), _} <- get()].

%% @equiv set_headers(Headers, [])
-spec set_headers(headers()) -> logger_instance().
set_headers(Headers) -> set_headers(Headers, []).

-spec set_headers(headers(), Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}
               | {recursive, boolean()}.
set_headers(Headers, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Headers, Options]),
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    Recursive = proplists:get_value(recursive, Options, true),

    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:set_headers(Headers, IfExists, Logger0),

    Logger2 =
        case {Recursive, logi_logger:to_map(Logger1)} of
            {true, Map = #{next := Next0}} ->
                Next1 = set_headers(Headers, [{logger, Next0} | Options]),
                from_map(maps:put(next, Next1, Map));
            _ ->
                Logger1
        end,
    save_if_need(Need, Logger2).

-spec set_metadata(metadata()) -> logger_instance().
set_metadata(Metadata) -> set_metadata(Metadata, []).

-spec set_metadata(metadata(), Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}
               | {recursive, boolean()}.
set_metadata(Metadata, Options) ->
    %% XXX: headersとコード重複
    _ = is_list(Options) orelse erlang:error(badarg, [Metadata, Options]),
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    Recursive = proplists:get_value(recursive, Options, true),

    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:set_metadata(Metadata, IfExists, Logger0),

    Logger2 =
        case {Recursive, logi_logger:to_map(Logger1)} of
            {true, Map = #{next := Next0}} ->
                Next1 = set_metadata(Metadata, [{logger, Next0} | Options]),
                from_map(maps:put(next, Next1, Map));
            _ ->
                Logger1
        end,
    save_if_need(Need, Logger2).

-spec delete_headers([term()]) -> logger_instance().
delete_headers(Keys) -> delete_headers(Keys, []).

-spec delete_headers([term()], Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {recursive, boolean()}. % TODO: remove(?)
delete_headers(Keys, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    Recursive = proplists:get_value(recursive, Options, true),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:delete_headers(Keys, Logger0),
    Logger2 =
        case {Recursive, to_map(Logger1)} of
            {true, Map = #{next := Next0}} ->
                Next1 = delete_headers(Keys, [{logger, Next0} | Options]),
                from_map(maps:put(next, Next1, Map));
            _ ->
                Logger1
        end,
    save_if_need(Need, Logger2).

-spec delete_metadata([term()]) -> logger_instance().
delete_metadata(Keys) -> delete_metadata(Keys, []).

-spec delete_metadata([term()], Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}.
delete_metadata(Keys, Options) ->
    %% XXX: headersとコード重複
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    Recursive = proplists:get_value(recursive, Options, true),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:delete_metadata(Keys, Logger0),
    Logger2 =
        case {Recursive, to_map(Logger1)} of
            {true, Map = #{next := Next0}} ->
                Next1 = delete_metadata(Keys, [{logger, Next0} | Options]),
                from_map(maps:put(next, Next1, Map));
            _ ->
                Logger1
        end,
    save_if_need(Need, Logger2).

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
%% @deprecated TODO
%%
%% TODO: private(?)
-spec log(severity(), io:format(), [term()], log_options()) -> logger_instance().
log(Severity, Format, FormatArgs, Options) ->
    %% TODO: application:get_env(warn_deprecated) orelse do_warn()
    _ = is_list(Options) orelse erlang:error(badarg, [Severity, Format, FormatArgs, Options]),
    DefaultLocation =
        case lists:keyfind(location, 1, Options) of
            false         -> logi_location:guess_location();
            {_, Location} -> Location
        end,
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    {Results, Logger1} = logi_logger:ready(Logger0, Severity, DefaultLocation, Options),
    ok = lists:foreach(fun ({Context, Sinks}) -> logi_logger:write(Sinks, Context, Format, FormatArgs) end, Results),
    save_if_need(Need, Logger1).

-spec debug(io:format()) -> logger_instance().
debug(Format) -> debug(Format, []).

-spec debug(io:format(), [term()]) -> logger_instance().
debug(Format, Args) -> debug(Format, Args, []).

-spec debug(io:format(), [term()], log_options()) -> logger_instance().
debug(Format, Args, Options) -> log(debug, Format, Args, Options).

-spec verbose(io:format()) -> logger_instance().
verbose(Format) -> verbose(Format, []).

-spec verbose(io:format(), [term()]) -> logger_instance().
verbose(Format, Args) -> verbose(Format, Args, []).

-spec verbose(io:format(), [term()], log_options()) -> logger_instance().
verbose(Format, Args, Options) -> log(verbose, Format, Args, Options).

-spec info(io:format()) -> logger_instance().
info(Format) -> info(Format, []).

-spec info(io:format(), [term()]) -> logger_instance().
info(Format, Args) -> info(Format, Args, []).

-spec info(io:format(), [term()], log_options()) -> logger_instance().
info(Format, Args, Options) -> log(info, Format, Args, Options).

-spec notice(io:format()) -> logger_instance().
notice(Format) -> notice(Format, []).

-spec notice(io:format(), [term()]) -> logger_instance().
notice(Format, Args) -> notice(Format, Args, []).

-spec notice(io:format(), [term()], log_options()) -> logger_instance().
notice(Format, Args, Options) -> log(notice, Format, Args, Options).

-spec warning(io:format()) -> logger_instance().
warning(Format) -> warning(Format, []).

-spec warning(io:format(), [term()]) -> logger_instance().
warning(Format, Args) -> warning(Format, Args, []).

-spec warning(io:format(), [term()], log_options()) -> logger_instance().
warning(Format, Args, Options) -> log(warning, Format, Args, Options).

-spec error(io:format()) -> logger_instance().
error(Format) -> logi:error(Format, []).

-spec error(io:format(), [term()]) -> logger_instance().
error(Format, Args) -> logi:error(Format, Args, []).

-spec error(io:format(), [term()], log_options()) -> logger_instance().
error(Format, Args, Options) -> log(error, Format, Args, Options).

-spec critical(io:format()) -> logger_instance().
critical(Format) -> critical(Format, []).

-spec critical(io:format(), [term()]) -> logger_instance().
critical(Format, Args) -> critical(Format, Args, []).

-spec critical(io:format(), [term()], log_options()) -> logger_instance().
critical(Format, Args, Options) -> log(critical, Format, Args, Options).

-spec alert(io:format()) -> logger_instance().
alert(Format) -> alert(Format, []).

-spec alert(io:format(), [term()]) -> logger_instance().
alert(Format, Args) -> alert(Format, Args, []).

-spec alert(io:format(), [term()], log_options()) -> logger_instance().
alert(Format, Args, Options) -> log(alert, Format, Args, Options).

-spec emergency(io:format()) -> logger_instance().
emergency(Format) -> emergency(Format, []).

-spec emergency(io:format(), [term()]) -> logger_instance().
emergency(Format, Args) -> emergency(Format, Args, []).

-spec emergency(io:format(), [term()], log_options()) -> logger_instance().
emergency(Format, Args, Options) -> log(emergency, Format, Args, Options).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec save_if_need({ok, logger_id()} | error, logger_instance()) -> logger_instance().
save_if_need(error,    Logger) -> Logger;
save_if_need({ok, Id}, Logger) -> _ = save(Id, Logger), Logger.

-spec load_if_need(logger()) -> {{ok, logger_id()} | error, logger_instance()}.
load_if_need(Logger) when is_atom(Logger) -> {{ok, Logger}, load_or_new(Logger)};
load_if_need(Logger)                      -> {error, Logger}.
