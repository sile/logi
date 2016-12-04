%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Logger Interface
%%
%% This module mainly provides logger related functions.
%% A logger has own headers, metadata, filter and can issue log messages to a destination channel.
%%
%% == EXAMPLE ==
%% Basic Usage:
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warnings for the sake of brevity
%% > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(foo), info). % Installs a sink to the default channel
%% > logi:info("hello world").
%% 2015-11-09 08:18:34.954 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
%% @end
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
-export([new/0, new/1]).
-export([is_logger/1]).
-export([to_map/1, from_map/1]).
-export([to_list/1, from_list/1]).

-export([set_headers/1, set_headers/2]).
-export([set_metadata/1, set_metadata/2]).
-export([delete_headers/1, delete_headers/2]).
-export([delete_metadata/1, delete_metadata/2]).

-export([save_as_default/1, save/2]).
-export([load_default/0, load/1]).
-export([ensure_to_be_instance/1]).
-export([erase/0, erase/1]).
-export([which_loggers/0]).

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
-export([log/4]).
-export([debug/1, debug/2, debug/3]).
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
-export_type([severity/0]).
-export_type([logger/0, logger_id/0, logger_instance/0]).
-export_type([logger_map_form/0]).
-export_type([new_option/0, new_options/0]).

-export_type([headers/0, metadata/0]).
-export_type([log_option/0, log_options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export(['_ready'/3, '_write'/3]). % for `logi_transform'

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type severity()  :: debug | info | notice | warning | error | critical | alert | emergency.
%% Severity of a log message
%%
%% It follwed the severities which are described in [RFC 5424](https://tools.ietf.org/html/rfc5424#section-6.2.1).

-type logger() :: logger_id() | logger_instance().
%% A logger

-type logger_id() :: atom().
%% The ID of a saved logger instance (see: {@link save/2}).
%%
%% If such a logger instance does not exist,
%% the ID will be regarded as an alias of the expression `new([{channel, LoggerId}])'.

-opaque logger_instance() :: logi_logger:logger().
%% A logger instance

-type logger_map_form() ::
        #{
           channel  => logi_channel:id(),
           headers  => headers(),
           metadata => metadata(),
           filter   => logi_filter:filter(),
           next     => logger_instance()
         }.
%% The map representation of a logger.
%%
%% `filter' and `next' fields are optional
%% (e.g. If a logger has no filter, the `filter' field is omitted from the corresponding map).

-type headers() :: #{}.
%% The headers of a log message.
%%
%% Headers are intended to be included in the outputs written by sinks.

-type metadata() :: #{}.
%% The metadata of a log message
%%
%% Metadata are not intended to be included directly in the outputs written by sinks.
%% The main purpose of metadata is to provide means to convey information from the log issuer to filters or sinks.

-type new_options() :: [new_option()].
-type new_option() :: {channel, logi_channel:id()}
                    | {headers, headers()}
                    | {metadata, metadata()}
                    | {filter, logi_filter:filter()}
                    | {next, logger_instance()}.
%% [channel]
%% - The destination channel
%% - The log messages issued by the created logger will (logically) send to the channel
%% - Default: `logi_channel:default_channel()'
%%
%% [headers]
%% - The headers of the created logger
%% - Default: `#{}'
%%
%% [metadata]
%% - The metadata of the created logger
%% - Default: `#{}'
%%
%% [filter]
%% - A log message filter
%% - Default: none (optional)
%%
%% [next]
%% - A next logger
%% - An application of the some function (e.g. {@link log/4}) to the created logger is also applied to the next logger
%% - Default: none (optional)

-type log_options() :: [log_option()].
-type log_option() :: {logger, logger()}
                    | {location, logi_location:location()}
                    | {headers, headers()}
                    | {metadata, metadata()}
                    | {timestamp, erlang:timestamp()}.
%% [logger]
%% - The logger of interest
%% - Default: `logi:default_logger()'
%%
%% [location]
%% - The log message issued location
%% - Default: `logi_location:guess_location()'
%%
%% [headers]
%% - The headers of the log message
%% - They are merged with the headers of the logger (the former has priority when key collisions occur)
%% - Default: `#{}'
%%
%% [metadata]
%% - The metadata of the log message
%% - They are merged with the metadata of the logger (the former has priority when key collisions occur)
%% - Default: `#{}'
%%
%% [timestamp]
%% - The log message issued time
%% - Default: `os:timestamp()'


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
%% The default channel {@link logi_channel:default_channel/0} which corresponds to the logger
%% is started automatically when `logi' application was started.
-spec default_logger() -> logger_id().
default_logger() -> logi_default_log.

%% @doc Returns the available severity list
%%
%% The list are ordered by the their severity level (see: {@link severity_level/1}).
-spec severities() -> [severity()].
severities() -> [emergency, alert, critical, error, warning, notice, info, debug].

%% @doc Returns the level of `Severity'
%%
%% The higher the severity is, the lower the level is.
-spec severity_level(Severity :: severity()) -> 1..8.
severity_level(emergency) -> 1;
severity_level(alert)     -> 2;
severity_level(critical)  -> 3;
severity_level(error)     -> 4;
severity_level(warning)   -> 5;
severity_level(notice)    -> 6;
severity_level(info)      -> 7;
severity_level(debug)     -> 8;
severity_level(Severity)  -> erlang:error(badarg, [Severity]).

%% @doc Returns `true' if `X' is a severity, otherwise `false'
-spec is_severity(X :: (severity() | term())) -> boolean().
is_severity(X) -> lists:member(X, severities()).

%%----------------------------------------------------------
%% Logger
%%----------------------------------------------------------
%% @equiv new([])
-spec new() -> logger_instance().
new() -> new([]).

%% @doc Creates a new logger instance
-spec new(new_options()) -> logger_instance().
new(Options) -> logi_logger:new(Options).

%% @doc Returns `true' if `X' is a logger, otherwise `false'
-spec is_logger(X :: (logger() | term())) -> boolean().
is_logger(X) -> is_atom(X) orelse logi_logger:is_logger(X).

%% @doc Converts `Logger' into a map form
%%
%% The optional entries (i.e. `filter' and `next') will be omitted from the resulting map if the value is not set.
%%
%% <pre lang="erlang">
%% > logi:to_map(logi:new()).
%% #{channel => logi_default_log,headers => #{},metadata => #{}}
%%
%% > logi:to_map(logi:new([{next, logi:new()}])).
%% #{channel => logi_default_log,
%%   headers => #{},
%%   metadata => #{},
%%   next => {logi_logger,logi_default_log,#{},#{},undefined,undefined}}
%% </pre>
-spec to_map(logger()) -> logger_map_form().
to_map(Logger) -> logi_logger:to_map(element(2, load_if_need(Logger))).

%% @doc Creates a new logger instance from `Map'
%%
%% Default Value:
%% - channel: `logi_channel:default_channel()'
%% - headers: `#{}'
%% - metadata: `#{}'
%% - filter: none (optional)
%% - next: none (optional)
%%
%% <pre lang="erlang">
%% > logi:to_map(logi:from_map(#{})).
%% #{channel => logi_default_log,headers => #{},metadata => #{}}
%% </pre>
-spec from_map(logger_map_form()) -> logger_instance().
from_map(Map) -> logi_logger:from_map(Map).

%% @doc Flattens the nested logger
%%
%% The nested loggers are collected as a flat list.
%% The `next' fields of the resulting loggers are removed.
%%
%% <pre lang="erlang">
%% > Logger0 = logi:new().
%% > Logger1 = logi:new([{next, Logger0}]).
%% > Logger2 = logi:new([{next, Logger1}]).
%% > [Logger0] = logi:to_list(Logger0).
%% > [Logger0, Logger0] = logi:to_list(Logger1).
%% > [Logger0, Logger0, Logger0] = logi:to_list(Logger2).
%% </pre>
-spec to_list(logger()) -> [logger_instance()].
to_list(Logger) ->
    Map = to_map(Logger),
    case Map of
        #{next := Next} -> [from_map(maps:remove(next, Map)) | to_list(Next)];
        _               -> [from_map(Map)]
    end.

%% @doc Aggregates `Loggers' into a logger instance
%%
%% The head logger in `Loggers' becomes the root of the aggregation.
%%
%% e.g. `from_list([new(), new(), new()])' is equivalent to `new([{next, new([{next, new()}])}])'.
%%
%% <pre lang="erlang">
%% > GetChannel = fun (Logger) -> maps:get(channel, logi:to_map(Logger)) end.
%%
%% > Logger0 = logi:new([{channel, aaa}]).
%% > Logger1 = logi:new([{channel, bbb}]).
%% > Logger2 = logi:new([{channel, ccc}, {next, logi:new([{channel, ccc_sub}])}]).
%%
%% > [aaa, bbb] = lists:map(GetChannel, logi:to_list(logi:from_list([Logger0, Logger1]))).
%% > [ccc, ccc_sub, aaa, bbb] = lists:map(GetChannel, logi:to_list(logi:from_list([Logger2, Logger0, Logger1]))).
%% </pre>
-spec from_list(Loggers :: [logger()]) -> logger_instance().
from_list([])      -> erlang:error(badarg, [[]]);
from_list(Loggers) ->
    lists:foldr(
      fun (Parent, undefined) -> element(2, load_if_need(Parent));
          (Parent, Child0)    ->
              ParentMap = to_map(Parent),
              Child1 =
                  case ParentMap of
                      #{next := Next} -> from_list([Next, Child0]);
                      _               -> Child0
                  end,
              from_map(maps:put(next, Child1, ParentMap))
      end,
      undefined,
      Loggers).

%% @equiv save(default_logger(), Logger)
-spec save_as_default(logger()) -> logger_instance() | undefined.
save_as_default(Logger) -> save(default_logger(), Logger).

%% @doc Saves `Logger' with the ID `LoggerId' to the process dictionary
%%
%% If `LoggerId' already exists, the old logger instance is deleted and replaced by `Logger'
%% and the function returns the old instance.
%% Otherwise it returns `undefined'.
%%
%% In the process, a saved logger instance can be referred by the ID.
%% <pre lang="erlang">
%% > Logger = logi:new().
%% > logi:save(sample_log, Logger).
%%
%% % The following two expression is equivalent.
%% > logi:info("hello world", [{logger, Logger}]).  % referred by instance
%% > logi:info("hello world", [{logger, sample_log}]). % referred by ID
%% </pre>
-spec save(logger_id(), Logger :: logger()) -> logger_instance() | undefined.
save(LoggerId, Logger0) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId, Logger0]),
    _ = is_logger(Logger0) orelse erlang:error(badarg, [LoggerId, Logger0]),
    Logger1 =
        case is_atom(Logger0) of
            true  -> load_or_new(Logger0);
            false -> Logger0
        end,
    put(?PD_LOGGER_KEY(LoggerId), Logger1).

%% @equiv load(default_logger())
-spec load_default() -> {ok, logger_instance()} | error.
load_default() -> load(default_logger()).

%% @doc Loads a logger which associated with the ID `LoggerId' from the process dictionary
%%
%% <pre lang="erlang">
%% > error = logi:load(hoge_log).
%%
%% > logi:save(hoge_log, logi:new()).
%% > {ok, _} = logi:load(hoge_log).
%% </pre>
-spec load(logger_id()) -> {ok, logger_instance()} | error.
load(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    case get(?PD_LOGGER_KEY(LoggerId)) of
        undefined -> error;
        Logger    -> {ok, Logger}
    end.

%% @doc Returns the logger instance associated to `Logger'
%%
%% <pre lang="erlang">
%% > logi:ensure_to_be_instance(unsaved).
%% {logi_logger,unsaved,#{},#{},undefined,undefined}
%%
%% > logi:save(saved, logi:new([{channel, hoge}])).
%% > logi:ensure_to_be_instance(hoge).
%% {logi_logger,hoge,#{},#{},undefined,undefined}
%%
%% > logi:ensure_to_be_instance(logi:new([{channel, instance}])).
%% {logi_logger,instance,#{},#{},undefined,undefined}
%% </pre>
-spec ensure_to_be_instance(logger()) -> logger_instance().
ensure_to_be_instance(Logger) when is_atom(Logger) -> load_or_new(Logger);
ensure_to_be_instance(Logger)                      -> Logger.

%% @doc Returns the saved loggers and deletes them from the process dictionary.
%%
%% <pre lang="erlang">
%% > logi:save(hoge, logi:new()).
%% > logi:erase().
%% [{hoge,{logi_logger,logi_default_log,#{},#{},undefined,undefined}}]
%%
%% > logi:erase().
%% []
%% </pre>
-spec erase() -> [{logger_id(), logger_instance()}].
erase() -> [{Id, logi:erase(Id)} || Id <- which_loggers()].

%% @doc Returns the logger associated with `LoggerId' and deletes it from the process dictionary.
%%
%% Returns `undefined' if no logger is associated with `LoggerId'.
%%
%% <pre lang="erlang">
%% > logi:save(hoge, logi:new()).
%% > logi:erase(hoge).
%% {logi_logger,logi_default_log,#{},#{},undefined,undefined}
%%
%% > logi:erase(hoge).
%% undefined
%% </pre>
-spec erase(logger_id()) -> logger_instance() | undefined.
erase(LoggerId) ->
    _ = is_atom(LoggerId) orelse erlang:error(badarg, [LoggerId]),
    erlang:erase(?PD_LOGGER_KEY(LoggerId)).

%% @doc Returns the ID list of the saved loggers
%%
%% <pre lang="erlang">
%% > logi:save(hoge, logi:new()).
%% > logi:which_loggers().
%% [hoge]
%% </pre>
-spec which_loggers() -> [logger_id()].
which_loggers() -> [LoggerId || {?PD_LOGGER_KEY(LoggerId), _} <- get()].

%% @equiv set_headers(Headers, [])
-spec set_headers(headers()) -> logger_instance().
set_headers(Headers) -> set_headers(Headers, []).

%% @doc Sets headers of the logger
%%
%% If the logger has nested loggers, the function is applied to them recursively.
%%
%% === OPTION ===
%% [logger]
%% - The logger to which the operation applies.
%% - Default: `logi:default_logger()'.
%%
%% [if_exists]
%% - If the value is `supersede', the existing headers are deleted and replaced by `Headers'.
%% - If the value is `overwrite', the existing headers and `Headers' are merged and the rear has priority when a key collision occurs.
%% - If the value is `ignore', the existing headers and `Headers' are merged and the former has priority when a key collision occurs.
%% - Default: `overwrite'
%%
%% <pre lang="erlang">
%% > Logger = logi:new([{headers, #{a => 10, b => 20}}]).
%% > Set = fun (Headers, IfExists) ->
%%             L = logi:set_headers(Headers, [{logger, Logger}, {if_exists, IfExists}]),
%%             maps:get(headers, logi:to_map(L))
%%         end.
%%
%% > true = #{a => 0,           c => 30} =:= Set(#{a => 0, c => 30}, supersede).
%% > true = #{a => 0,  b => 20, c => 30} =:= Set(#{a => 0, c => 30}, overwrite).
%% > true = #{a => 10, b => 20, c => 30} =:= Set(#{a => 0, c => 30}, ignore).
%% </pre>
-spec set_headers(headers(), Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}.
set_headers(Headers, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Headers, Options]),
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:recursive_update(fun (L) -> logi_logger:set_headers(Headers, IfExists, L) end, Logger0),
    save_if_need(Need, Logger1).

%% @equiv set_metadata(Metadata, [])
-spec set_metadata(metadata()) -> logger_instance().
set_metadata(Metadata) -> set_metadata(Metadata, []).

%% @doc Sets metadata of the logger
%%
%% If the logger has nested loggers, the function is applied to them recursively.
%%
%% === OPTION ===
%% [logger]
%% - The logger to which the operation applies.
%% - Default: `logi:default_logger()'.
%%
%% [if_exists]
%% - If the value is `supersede', the existing metadata is deleted and replaced by `Metadata'.
%% - If the value is `overwrite', the existing metadata and `Metadata' are merged and the rear has priority when a key collision occurs.
%% - If the value is `ignore', the existing metadata and `Metadata' are merged and the former has priority when a key collision occurs.
%% - Default: `overwrite'
%%
%% <pre lang="erlang">
%% > Logger = logi:new([{metadata, #{a => 10, b => 20}}]).
%% > Set = fun (Metadata, IfExists) ->
%%             L = logi:set_metadata(Metadata, [{logger, Logger}, {if_exists, IfExists}]),
%%             maps:get(metadata, logi:to_map(L))
%%         end.
%%
%% > true = #{a => 0,           c => 30} =:= Set(#{a => 0, c => 30}, supersede).
%% > true = #{a => 0,  b => 20, c => 30} =:= Set(#{a => 0, c => 30}, overwrite).
%% > true = #{a => 10, b => 20, c => 30} =:= Set(#{a => 0, c => 30}, ignore).
%% </pre>
-spec set_metadata(metadata(), Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}
               | {if_exists, ignore | overwrite | supersede}.
set_metadata(Metadata, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Metadata, Options]),
    IfExists = proplists:get_value(if_exists, Options, overwrite),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:recursive_update(fun (L) -> logi_logger:set_metadata(Metadata, IfExists, L) end, Logger0),
    save_if_need(Need, Logger1).

%% @equiv delete_headers(Keys, [])
-spec delete_headers([term()]) -> logger_instance().
delete_headers(Keys) -> delete_headers(Keys, []).

%% @doc Deletes headers which associated with `Keys'
%%
%% If the logger has nested loggers, the function is applied to them recursively.
%%
%% === OPTION ===
%% [logger]
%% - The logger to which the operation applies.
%% - Default: `logi:default_logger()'.
%%
%% <pre lang="erlang">
%% > Logger = logi:new([{headers, #{a => 1, b => 2}}]).
%% > logi:to_map(logi:delete_headers([a], [{logger, Logger}])).
%% #{channel => logi_default_log,headers => #{b => 2},metadata => #{}}
%% </pre>
-spec delete_headers([term()], Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}.
delete_headers(Keys, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:recursive_update(fun (L) -> logi_logger:delete_headers(Keys, L) end, Logger0),
    save_if_need(Need, Logger1).

%% @equiv delete_metadata(Keys, [])
-spec delete_metadata([term()]) -> logger_instance().
delete_metadata(Keys) -> delete_metadata(Keys, []).

%% @doc Deletes metadata entries which associated with `Keys'
%%
%% If the logger has nested loggers, the function is applied to them recursively.
%%
%% === OPTION ===
%% [logger]
%% - The logger to which the operation applies.
%% - Default: `logi:default_logger()'.
%%
%% <pre lang="erlang">
%% > Logger = logi:new([{metadata, #{a => 1, b => 2}}]).
%% > logi:to_map(logi:delete_metadata([a], [{logger, Logger}])).
%% #{channel => logi_default_log,headers => #{},metadata => #{b => 2}}
%% </pre>
-spec delete_metadata([term()], Options) -> logger_instance() when
      Options :: [Option],
      Option  :: {logger, logger()}.
delete_metadata(Keys, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Keys, Options]),
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    Logger1 = logi_logger:recursive_update(fun (L) -> logi_logger:delete_metadata(Keys, L) end, Logger0),
    save_if_need(Need, Logger1).

%%----------------------------------------------------------
%% Logging
%%----------------------------------------------------------
%% @doc Issues a log message to the destination channel.
%%
%% If the logger has a filter, the message will be passed to it.
%% And if the message has not been discarded by a filter,
%% the logger will (logically) send it to the destination channel.
%% Finally, the message will be consumed by the sinks which are installed to the channel.
%% But the sinks which does not satisfy specified condition (i.e. `logi_sink:condition/0') are ignored.
%%
%% <pre lang="erlang">
%% > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(sample), info). % Installs a sink to the default channel
%% > logi:log(debug, "hello world", [], []). % There are no applicable sinks (the severity is too low)
%% > logi:log(info, "hello world", [], []). % The log message is consumed by the above sink
%% 2015-10-22 13:16:37.003 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%  </pre>
%%
%% If the logger has nested loggers, the function is applied to them recursively.
%%
%% <pre lang="erlang">
%% > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(sample), info). % Installs a sink to the default channel
%% > Logger = logi:from_list([logi:new([{headers, #{id => hoge}}]), logi:new([{headers, #{id => fuga}}])]).
%% > logi:log(info, "hello world", [], [{logger, Logger}]).
%% 2015-10-22 13:28:10.332 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [id=hoge] hello world
%% 2015-10-22 13:28:10.332 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [id=fuga] hello world
%% </pre>
%%
%% === NOTE ===
%%
%% Typically, it is preferred to log messages through the wrapper functions (i.e. `logi:Severity/{1,2,3}')
%% rather than calling the function directly.
%%
%% If the `{parse_transform, logi_transform}' compiler option is specified,
%% the invocation of the wrapper functions will be transformed to a more efficient code at compile time.
%%
%% For example `logi:info("hello world)' will be transformed to a code such as following:
%% <pre lang="erlang">
%% %% Current location (`Application', `Module', `Function' and `Line') is detected at compile time
%% Location = logi_location:unsafe_new(self(), Application, Module, Function, Line),
%% case logi:'_ready'(info, Location, Options) of
%%   {Logger, []}                    -> Logger;
%%   {Logger, ListOfContextAndSinks} -> logi:`_write'(ListOfContextAndSinks, Format, Data)
%% end.
%% </pre>
%%
%% From the efficiency point of view, the following two points are important:
%% - 1. An implicit call of {@link logi_location:guess_location/0} to guess the currrent location is replaced by the more efficient and accurate code
%% - 2. If it is unnecessary (e.g. there are no applicable sinks), `Format' and `Data' will not be evaluated
%%
%% The {@link logi_location:guess_location/0} is a heavy function,
%% so if it is called at runtime, a warning will be emitted via the `error_logger' module.
%%
%% @see logi_location:guess_location/0
-spec log(severity(), io:format(), [term()], log_options()) -> logger_instance().
log(Severity, Format, Data, Options) ->
    _ = is_list(Options) orelse erlang:error(badarg, [Severity, Format, Data, Options]),
    DefaultLocation =
        case lists:keyfind(location, 1, Options) of
            false         -> logi_location:guess_location();
            {_, Location} -> Location
        end,
    {Logger1, Result} = '_ready'(Severity, DefaultLocation, Options),
    _ = '_write'(Result, Format, Data),
    Logger1.

%% @equiv debug(Format, [])
-spec debug(io:format()) -> logger_instance().
debug(Format) -> debug(Format, []).

%% @equiv debug(Format, Data, [])
-spec debug(io:format(), [term()]) -> logger_instance().
debug(Format, Data) -> debug(Format, Data, []).

%% @equiv log(debug, Format, Data, Options)
-spec debug(io:format(), [term()], log_options()) -> logger_instance().
debug(Format, Data, Options) -> log(debug, Format, Data, Options).

%% @equiv info(Format, [])
-spec info(io:format()) -> logger_instance().
info(Format) -> info(Format, []).

%% @equiv info(Format, Data, [])
-spec info(io:format(), [term()]) -> logger_instance().
info(Format, Data) -> info(Format, Data, []).

%% @equiv log(info, Format, Data, Options)
-spec info(io:format(), [term()], log_options()) -> logger_instance().
info(Format, Data, Options) -> log(info, Format, Data, Options).

%% @equiv notice(Format, [])
-spec notice(io:format()) -> logger_instance().
notice(Format) -> notice(Format, []).

%% @equiv notice(Format, Data, [])
-spec notice(io:format(), [term()]) -> logger_instance().
notice(Format, Data) -> notice(Format, Data, []).

%% @equiv log(notice, Format, Data, Options)
-spec notice(io:format(), [term()], log_options()) -> logger_instance().
notice(Format, Data, Options) -> log(notice, Format, Data, Options).

%% @equiv warning(Format, [])
-spec warning(io:format()) -> logger_instance().
warning(Format) -> warning(Format, []).

%% @equiv warning(Format, Data, [])
-spec warning(io:format(), [term()]) -> logger_instance().
warning(Format, Data) -> warning(Format, Data, []).

%% @equiv log(warning, Format, Data, Options)
-spec warning(io:format(), [term()], log_options()) -> logger_instance().
warning(Format, Data, Options) -> log(warning, Format, Data, Options).

%% @equiv error(Format, [])
-spec error(io:format()) -> logger_instance().
error(Format) -> logi:error(Format, []).

%% @equiv error(Format, Data, [])
-spec error(io:format(), [term()]) -> logger_instance().
error(Format, Data) -> logi:error(Format, Data, []).

%% @equiv log(error, Format, Data, Options)
-spec error(io:format(), [term()], log_options()) -> logger_instance().
error(Format, Data, Options) -> log(error, Format, Data, Options).

%% @equiv critical(Format, [])
-spec critical(io:format()) -> logger_instance().
critical(Format) -> critical(Format, []).

%% @equiv critical(Format, Data, [])
-spec critical(io:format(), [term()]) -> logger_instance().
critical(Format, Data) -> critical(Format, Data, []).

%% @equiv log(critical, Format, Data, Options)
-spec critical(io:format(), [term()], log_options()) -> logger_instance().
critical(Format, Data, Options) -> log(critical, Format, Data, Options).

%% @equiv alert(Format, [])
-spec alert(io:format()) -> logger_instance().
alert(Format) -> alert(Format, []).

%% @equiv alert(Format, Data, [])
-spec alert(io:format(), [term()]) -> logger_instance().
alert(Format, Data) -> alert(Format, Data, []).

%% @equiv log(alert, Format, Data, Options)
-spec alert(io:format(), [term()], log_options()) -> logger_instance().
alert(Format, Data, Options) -> log(alert, Format, Data, Options).

%% @equiv emergency(Format, [])
-spec emergency(io:format()) -> logger_instance().
emergency(Format) -> emergency(Format, []).

%% @equiv emergency(Format, Data, [])
-spec emergency(io:format(), [term()]) -> logger_instance().
emergency(Format, Data) -> emergency(Format, Data, []).

%% @equiv log(emergency, Format, Data, Options)
-spec emergency(io:format(), [term()], log_options()) -> logger_instance().
emergency(Format, Data, Options) -> log(emergency, Format, Data, Options).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec '_ready'(severity(), logi_location:location(), log_options()) ->
                      {logger_instance(), [{logi_context:context(), logi_sink_table:select_result()}]}.
'_ready'(Severity, DefaultLocation, Options) ->
    {Need, Logger0} = load_if_need(proplists:get_value(logger, Options, default_logger())),
    {Result, Logger1} = logi_logger:ready(Logger0, Severity, DefaultLocation, Options),
    {save_if_need(Need, Logger1), Result}.

%% @private
-spec '_write'([{logi_context:context(), logi_sink_table:select_result()}], io:format(), [term()]) -> ok.
'_write'([], _Format, _Data) ->
    ok;
'_write'([{Context, Sinks} | Rest], Format, Data) ->
    _ = logi_logger:write(Sinks, Context, Format, Data),
    '_write'(Rest, Format, Data).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec save_if_need({ok, logger_id()} | error, logger_instance()) -> logger_instance().
save_if_need(error,    Logger) -> Logger;
save_if_need({ok, Id}, Logger) -> _ = save(Id, Logger), Logger.

-spec load_if_need(logger()) -> {{ok, logger_id()} | error, logger_instance()}.
load_if_need(Logger) when is_atom(Logger) -> {{ok, Logger}, load_or_new(Logger)};
load_if_need(Logger)                      -> {error, Logger}.

-spec load_or_new(logger_id()) -> logger_instance().
load_or_new(LoggerId) ->
    case load(LoggerId) of
        error        -> new([{channel, LoggerId}]);
        {ok, Logger} -> Logger
    end.
