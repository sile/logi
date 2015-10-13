%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_logger).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_logger/1]).
-export([to_map/1, from_map/1]).

-export([set_headers/3]).
-export([delete_headers/2]).
-export([set_metadata/3]).
-export([delete_metadata/2]).

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
          channel_id     :: logi_channel:id(),
          headers  = #{} :: logi:headers(),
          metadata = #{} :: logi:metadata(),
          filter         :: undefined | logi_filter:filter(),
          next           :: undefined | logger()
        }).

-opaque logger() :: #?LOGGER{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(ChannelId, [])
-spec new(logi_channel:id()) -> logger().
new(ChannelId) -> new(ChannelId, []).

-spec new(logi_channel:id(), logi:new_options()) -> logger().
new(ChannelId, Options) ->
    Args = [ChannelId, Options],
    _ = is_atom(ChannelId) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),
    Headers  = ?GET_AND_VALIDATE(headers, Options, #{}, fun erlang:is_map/1, Args),
    Metadata = ?GET_AND_VALIDATE(metadata, Options, #{}, fun erlang:is_map/1, Args),
    Filter   = ?GET_AND_VALIDATE(filter, Options, undefined, fun logi_filter:is_filter/1, Args),
    Next     = ?GET_AND_VALIDATE(next, Options, undefined, fun is_logger/1, Args),
    #?LOGGER{
        channel_id = ChannelId,
        headers    = Headers,
        metadata   = Metadata,
        filter     = Filter,
        next       = Next
       }.

%% @doc Returns `true' if `X' is a logger, `false' otherwise
-spec is_logger(X :: (logger() | term())) -> boolean().
is_logger(X) -> is_record(X, ?LOGGER).

-spec to_map(logger()) -> logi:logger_map().
to_map(L = #?LOGGER{}) ->
    maps:filter(
      fun is_not_default/2,
      #{
         channel_id => L#?LOGGER.channel_id,
         headers    => L#?LOGGER.headers,
         metadata   => L#?LOGGER.metadata,
         filter     => L#?LOGGER.filter,
         next       => L#?LOGGER.next
       }).

-spec from_map(logi:logger_map()) -> logger().
from_map(Map = #{channel_id := ChannelId}) -> new(ChannelId, maps:to_list(Map));
from_map(Map)                              -> error(badarg, [Map]).

-spec set_headers(logi:headers(), ignore | overwrite | supersede, logger()) -> logger().
set_headers(Headers, ignore,    Logger) -> Logger#?LOGGER{headers = maps:merge(Headers, Logger#?LOGGER.headers)};
set_headers(Headers, overwrite, Logger) -> Logger#?LOGGER{headers = maps:merge(Logger#?LOGGER.headers, Headers)};
set_headers(Headers, supersede, Logger) -> Logger#?LOGGER{headers = Headers}.

-spec delete_headers([term()], logger()) -> logger().
delete_headers(Keys, Logger) -> Logger#?LOGGER{headers = maps:without(Keys, Logger#?LOGGER.headers)}.

-spec set_metadata(logi:metadata(), ignore | overwrite | supersede, logger()) -> logger().
set_metadata(Metadata, ignore,    Logger) -> Logger#?LOGGER{metadata = maps:merge(Metadata, Logger#?LOGGER.metadata)};
set_metadata(Metadata, overwrite, Logger) -> Logger#?LOGGER{metadata = maps:merge(Logger#?LOGGER.metadata, Metadata)};
set_metadata(Metadata, supersede, Logger) -> Logger#?LOGGER{metadata = Metadata}.

-spec delete_metadata([term()], logger()) -> logger().
delete_metadata(Keys, Logger) -> Logger#?LOGGER{metadata = maps:without(Keys, Logger#?LOGGER.metadata)}.

%% TODO: set_filter

%% XXX: name: Result
-spec ready(logger(), logi:severity(), logi_location:location(), logi:log_options()) -> {[Result], logger()} when
      Result :: {logi_context:context(), [logi_sink:sink()]}.
ready(Logger0, Severity, DefaultLocation, Options) ->
    Args = [Logger0, Severity, DefaultLocation, Options],
    _ = is_list(Options) orelse error(badarg, Args),
    Location = ?GET_AND_VALIDATE(location, Options, DefaultLocation, fun logi_location:is_location/1, Args),
    Subject = proplists:get_value(subject, Options, undefined),
    Timestamp = proplists:get_value(timestamp, Options, undefined),
    ready(Logger0, Severity, Subject, Location, undefined, undefined, Timestamp, Options).

%% TODO: spec
%% TODO: refactoring
ready(undefined, _, _, _, _, _, _, _) ->
    {[], undefined};
ready(Logger0, Severity, Subject, Location, Headers0, Metadata0, Timestamp0, Options) ->
    Sinks = logi_channel:select_sink(Logger0#?LOGGER.channel_id, Severity,
                                     logi_location:get_application(Location), logi_location:get_module(Location)),
    case Sinks of
        [] ->
            {Results, Next} = ready(Logger0#?LOGGER.next, Severity, Subject, Location, Headers0, Metadata0, Timestamp0, Options),
            {Results, Logger0#?LOGGER{next = Next}};
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
                logi_context:unsafe_new(
                  Logger0#?LOGGER.channel_id, Timestamp1, Severity, Subject, Location, FullHeaders, FullMetadata),
            {Results, Next} = ready(Logger0#?LOGGER.next, Severity, Subject, Location, Headers1, Metadata1, Timestamp1, Options),
            case apply_filter(Context, Logger0#?LOGGER{next = Next}) of
                {false, Logger1} -> {Results, Logger1};
                {true,  Logger1} -> {[{Context, Sinks} | Results], Logger1}
            end
    end.

-spec write([logi_sink:sink()], logi_context:context(), io:format(), [term()]) -> ok.
write(Sinks, Context, Format, FormatArgs) ->
    lists:foreach(
      fun ({Module, ExtraData}) ->
              try
                  Module:write(Context, Format, FormatArgs, ExtraData)
              catch
                  Class:Reason ->
                      error_logger:error_report(
                        [{location, [{module, ?MODULE}, {line, ?LINE}, {pid, self()}]},
                         {mfargs, {Module, write, [Context, Format, FormatArgs, ExtraData]}},
                         {exception, {Class, Reason, erlang:get_stacktrace()}}])
              end
      end,
      Sinks).

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

-spec is_not_default(atom(), term()) -> boolean().
is_not_default(headers, V)        -> V =/= #{};
is_not_default(metadata, V)       -> V =/= #{};
is_not_default(filter, undefined) -> false;
is_not_default(next, undefined)   -> false;
is_not_default(_, _)              -> true.
