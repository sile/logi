%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Logger implementation module
%% @private
%% @end
-module(logi_logger).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).
-export([is_logger/1]).
-export([to_map/1, from_map/1]).

-export([set_headers/3]).
-export([set_metadata/3]).
-export([delete_headers/2]).
-export([delete_metadata/2]).
-export([recursive_update/2]).

-export([get_channel/1, get_headers/1, get_metadata/1, get_filter/1, get_next/1]).
-export([set_filter/2]).

-export([ready/4]).
-export([write/4]).

-export_type([logger/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(GET_AND_VALIDATE(Key, List, Default, ValidateFun, Args),
        (fun () ->
                 __Value = proplists:get_value(Key, List, Default),
                 case __Value =:= Default of
                     true  -> Default;
                     false ->
                         case ValidateFun(__Value) of
                             false -> error(badarg, Args);
                             true  -> __Value
                         end
                 end
         end)()).

-define(LOGGER, ?MODULE).

-record(?LOGGER,
        {
          channel        :: logi_channel:id(),
          headers  = #{} :: logi:headers(),
          metadata = #{} :: logi:metadata(),
          filter         :: undefined | logi_filter:filter(),
          next           :: undefined | logger()
        }).

-opaque logger() :: #?LOGGER{}.
%% A logger instance

-ifdef('FUN_STACKTRACE').
-define(CAPTURE_STACKTRACE, ).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-else.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new logger instance
-spec new(logi:new_options()) -> logger().
new(Options) ->
    Args = [Options],
    _ = is_list(Options) orelse error(badarg, Args),
    Channel  = ?GET_AND_VALIDATE(channel, Options, logi_channel:default_channel(), fun erlang:is_atom/1, Args),
    Headers  = ?GET_AND_VALIDATE(headers, Options, #{}, fun erlang:is_map/1, Args),
    Metadata = ?GET_AND_VALIDATE(metadata, Options, #{}, fun erlang:is_map/1, Args),
    Filter   = ?GET_AND_VALIDATE(filter, Options, undefined, fun logi_filter:is_filter/1, Args),
    Next     = ?GET_AND_VALIDATE(next, Options, undefined, fun is_logger/1, Args),
    #?LOGGER{
        channel = Channel,
        headers    = Headers,
        metadata   = Metadata,
        filter     = Filter,
        next       = Next
       }.

%% @doc Returns `true' if `X' is a logger, `false' otherwise
-spec is_logger(X :: (logger() | term())) -> boolean().
is_logger(X) -> is_record(X, ?LOGGER).

%% @doc Converts `Logger' into a map form
-spec to_map(Logger :: logger()) -> logi:logger_map_form().
to_map(L = #?LOGGER{}) ->
    maps:filter(
      fun is_not_undefined/2,
      #{
         channel  => L#?LOGGER.channel,
         headers  => L#?LOGGER.headers,
         metadata => L#?LOGGER.metadata,
         filter   => L#?LOGGER.filter,
         next     => L#?LOGGER.next
       }).

%% @doc Creates a new logger instance from `Map'
-spec from_map(logi:logger_map_form()) -> logger().
from_map(Map) ->
    _ = is_map(Map) orelse error(badarg, [Map]),
    new(maps:to_list(Map)).

%% @doc Gets the channel of `Logger'
-spec get_channel(Logger :: logger()) -> logi_channel:id().
get_channel(#?LOGGER{channel = Channel}) -> Channel.

%% @doc Gets the headers of `Logger'
-spec get_headers(Logger :: logger()) -> logi:headers().
get_headers(#?LOGGER{headers = Headers}) -> Headers.

%% @doc Gets the metadata of `Logger'
-spec get_metadata(Logger :: logger()) -> logi:metadata().
get_metadata(#?LOGGER{metadata = Metadata}) -> Metadata.

%% @doc Gets the filter of `Logger'
%%
%% If `Logger' does not have a filter, this function returns `error'.
-spec get_filter(Logger :: logger()) -> {ok, logi_filter:filter()} | error.
get_filter(#?LOGGER{filter = undefined}) -> error;
get_filter(#?LOGGER{filter = Filter})    -> {ok, Filter}.

%% NOTE: v0.0.12との互換性維持用関数
-spec set_filter(logger(), logi_filter:filter()) -> logger().
set_filter(Logger, Filter) ->
    Logger#?LOGGER{filter = Filter}.

%% @doc Gets the next logger of `Logger'
%%
%% If `Logger' does not have a next logger, this function returns `error'.
-spec get_next(Logger :: logger()) -> {ok, logger()} | error.
get_next(#?LOGGER{next = undefined}) -> error;
get_next(#?LOGGER{next = Next})      -> {ok, Next}.

%% @doc Sets the logger headers
-spec set_headers(logi:headers(), ignore | overwrite | supersede, logger()) -> logger().
set_headers(Headers, ignore,    Logger) -> Logger#?LOGGER{headers = maps:merge(Headers, Logger#?LOGGER.headers)};
set_headers(Headers, overwrite, Logger) -> Logger#?LOGGER{headers = maps:merge(Logger#?LOGGER.headers, Headers)};
set_headers(Headers, supersede, Logger) -> Logger#?LOGGER{headers = Headers}.

%% @doc Sets the logger metadata
-spec set_metadata(logi:metadata(), ignore | overwrite | supersede, logger()) -> logger().
set_metadata(Metadata, ignore,    Logger) -> Logger#?LOGGER{metadata = maps:merge(Metadata, Logger#?LOGGER.metadata)};
set_metadata(Metadata, overwrite, Logger) -> Logger#?LOGGER{metadata = maps:merge(Logger#?LOGGER.metadata, Metadata)};
set_metadata(Metadata, supersede, Logger) -> Logger#?LOGGER{metadata = Metadata}.

%% @doc Deletes the logger headers
-spec delete_headers([term()], logger()) -> logger().
delete_headers(Keys, Logger) -> Logger#?LOGGER{headers = maps:without(Keys, Logger#?LOGGER.headers)}.

%% @doc Deletes the logger metadata
-spec delete_metadata([term()], logger()) -> logger().
delete_metadata(Keys, Logger) -> Logger#?LOGGER{metadata = maps:without(Keys, Logger#?LOGGER.metadata)}.

%% @doc Updates the nested loggers recursively
-spec recursive_update(UpdateFun, logger()) -> logger() when
      UpdateFun :: fun ((logger()) -> logger()).
recursive_update(UpdateFun, Logger0) ->
    Logger1 = UpdateFun(Logger0),
    case Logger1 of
        #?LOGGER{next = undefined} -> Logger1;
        #?LOGGER{next = Next}      -> Logger1#?LOGGER{next = recursive_update(UpdateFun, Next)}
    end.

%% @doc Prepares the list of log output context and sink writers
-spec ready(logger(), logi:severity(), logi_location:location(), logi:log_options()) -> {[ContextAndWriters], logger()} when
      ContextAndWriters :: {logi_context:context(), logi_sink_table:select_result()}.
ready(Logger, Severity, DefaultLocation, Options) ->
    Args = [Logger, Severity, DefaultLocation, Options],
    _ = is_list(Options) orelse error(badarg, Args),
    Location = ?GET_AND_VALIDATE(location, Options, DefaultLocation, fun logi_location:is_location/1, Args),
    Timestamp = proplists:get_value(timestamp, Options, undefined),
    ready(Logger, Severity, Location, undefined, undefined, Timestamp, Options).

%% @doc Writes a log message through the selected sink writers
-spec write(logi_sink_table:select_result(), logi_context:context(), io:format(), [term()]) -> ok.
write([],                _Context,_Format,_Data) -> ok;
write([Writer | Writers], Context, Format, Data) ->
    %% An error of a writer does not affect to other writers. Instead, an error report is emitted.
    _ = try
            logi_sink_writer:write(Context, Format, Data, Writer)
        catch
            Class:Reason ?CAPTURE_STACKTRACE ->
                error_logger:error_report(
                  [{pid, self()}, {module, ?MODULE}, {line, ?LINE},
                   {msg, "logi_sink_writer:write/4 was aborted"},
                   {mfargs, {logi_sink_writer, write, [Context, Format, Data, Writer]}},
                   {exception, {Class, Reason, ?GET_STACKTRACE}}])
        end,
    write(Writers, Context, Format, Data).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec apply_filter(logi_context:context(), logger()) -> {boolean(), logger()}.
apply_filter(_Context, Logger = #?LOGGER{filter = undefined}) ->
    {true, Logger};
apply_filter(Context, Logger = #?LOGGER{filter = Filter0}) ->
    case logi_filter:apply(Context, Filter0) of
        {Result, Filter1} -> {Result, Logger#?LOGGER{filter = Filter1}};
        Result            -> {Result, Logger}
    end.

-spec is_not_undefined(atom(), term()) -> boolean().
is_not_undefined(filter, undefined) -> false;
is_not_undefined(next,   undefined) -> false;
is_not_undefined(_, _)              -> true.

-spec ready(undefined | logger(), logi:severity(), logi_location:location(),
            undefined | logi:headers(), undefined | logi:metadata(), undefined | erlang:timestamp(), logi:log_options()) ->
                   {[{logi_context:context(), [logi_sink_writer:writer()]}], undefined | logger()}.
ready(undefined, _, _, _, _, _, _) ->
    {[], undefined};
ready(Logger0, Severity, Location, Headers0, Metadata0, Timestamp0, Options) ->
    Writers = logi_channel:select_writer(Logger0#?LOGGER.channel, Severity,
                                         logi_location:get_application(Location), logi_location:get_module(Location)),
    case Writers of
        [] ->
            {Result, Next} = ready(Logger0#?LOGGER.next, Severity, Location, Headers0, Metadata0, Timestamp0, Options),
            {Result, Logger0#?LOGGER{next = Next}};
        _  ->
            Headers1 =
                case Headers0 of
                    undefined -> proplists:get_value(headers, Options, #{});
                    _         -> Headers0
                end,
            FullHeaders = maps:merge(Logger0#?LOGGER.headers, Headers1),

            Metadata1 =
                case Metadata0 of
                    undefined -> proplists:get_value(metadata, Options, #{});
                    _         -> Metadata0
                end,
            FullMetadata = maps:merge(Logger0#?LOGGER.metadata, Metadata1),

            Timestamp1 =
                case Timestamp0 of
                    undefined -> os:timestamp();
                    _         -> Timestamp0
                end,

            Context =
                logi_context:unsafe_new(Logger0#?LOGGER.channel, Timestamp1, Severity, Location, FullHeaders, FullMetadata),
            {Result, Next} = ready(Logger0#?LOGGER.next, Severity, Location, Headers1, Metadata1, Timestamp1, Options),
            case apply_filter(Context, Logger0#?LOGGER{next = Next}) of
                {false, Logger1} -> {Result, Logger1};
                {true,  Logger1} -> {[{Context, Writers} | Result], Logger1}
            end
    end.
