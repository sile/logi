%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_logger).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([to_map/1, from_map/1]).
-export([is_logger/1]).

-export([set_headers/3]).
-export([delete_headers/2]).
-export([set_metadata/3]).
-export([delete_metadata/2]).

-export([ready/3]).
-export([write/4]).

-export_type([logger/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(LOGGER, ?MODULE).

-record(?LOGGER,
        {
          channel_id      :: logi_channel:id(),
          headers  = none :: none | logi:headers(), % TODO: noneにすることでサイズが節約できるかどうか
          metadata = none :: none | logi:headers(),
          filters  = []   :: [logi_filter:filter()]
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
    Headers = get_and_validate(headers, Options, none, fun erlang:is_map/1),
    Headers =:= false andalso error(badarg, Args),
    Metadata = get_and_validate(metadata, Options, none, fun erlang:is_map/1),
    Metadata =:= false andalso error(badarg, Args),

    Logger =
        #?LOGGER{
            channel_id      = ChannelId,
            headers         = Headers, % TODO: #{}ならnoneにする
            metadata        = Metadata
           },
    Logger.

-spec is_logger(logger() | term()) -> boolean().
is_logger(X) -> is_record(X, ?LOGGER).

-spec to_map(logger()) -> logi:logger_map().
to_map(L = #?LOGGER{}) ->
    maps:put(
      channel_id, L#?LOGGER.channel_id,
      maps:filter(
        fun (_, V) -> V =/= none end,
        #{
           headers              => L#?LOGGER.headers,
           metadata             => L#?LOGGER.metadata,
           filters              => L#?LOGGER.filters
         })).

-spec from_map(logi:logger_map()) -> logger().
from_map(#{channel_id := ChannelId} = Map) ->
    new(ChannelId,
        [
         {headers, maps:get(headers, Map, none)},
         {metadata, maps:get(metadata, Map, none)},
         {filters, maps:get(filters, Map, [])}
        ]);
from_map(Map) ->
    error(badarg, [Map]).

-spec set_headers(logi:headers(), ignore | overwrite | supersede, logger()) -> logger().
set_headers(Headers, _, Logger = #?LOGGER{headers = none}) -> Logger#?LOGGER{headers = Headers};
set_headers(Headers, ignore,    Logger) -> Logger#?LOGGER{headers = maps:merge(Headers, Logger#?LOGGER.headers)};
set_headers(Headers, overwrite, Logger) -> Logger#?LOGGER{headers = maps:merge(Logger#?LOGGER.headers, Headers)};
set_headers(Headers, supersede, Logger) -> Logger#?LOGGER{headers = Headers}.

-spec delete_headers([term()], logger()) -> logger().
delete_headers(_Keys, Logger = #?LOGGER{headers = none}) -> Logger;
delete_headers(Keys, Logger) ->
    Logger#?LOGGER{headers = maps:without(Keys, Logger#?LOGGER.headers)}.

-spec set_metadata(logi:metadata(), ignore | overwrite | supersede, logger()) -> logger().
set_metadata(Metadata, _, Logger = #?LOGGER{metadata = none}) -> Logger#?LOGGER{metadata = Metadata};
set_metadata(Metadata, ignore,    Logger) -> Logger#?LOGGER{metadata = maps:merge(Metadata, Logger#?LOGGER.metadata)};
set_metadata(Metadata, overwrite, Logger) -> Logger#?LOGGER{metadata = maps:merge(Logger#?LOGGER.metadata, Metadata)};
set_metadata(Metadata, supersede, Logger) -> Logger#?LOGGER{metadata = Metadata}.

-spec delete_metadata([term()], logger()) -> logger().
delete_metadata(_Keys, Logger = #?LOGGER{metadata = none}) -> Logger;
delete_metadata(Keys, Logger) ->
    Logger#?LOGGER{metadata = maps:without(Keys, Logger#?LOGGER.metadata)}.

-spec ready(logi:severity(), logi_location:location(), logi:log_options()) -> Result when
      Result :: {[logi_sink:sink()], logi_context:context(), logger()} | logger().
ready(Severity, DefaultLocation, Options) ->
    _ = is_list(Options) orelse error(badarg, [Severity, Options]),
    Logger0 = logi:load_or_new(proplists:get_value(logger, Options, logi:default_logger())),
    Location = get_and_validate(location, Options, DefaultLocation, fun logi_location:is_location/1),
    Sinks = logi_channel:select_sink(Logger0#?LOGGER.channel_id, Severity,
                                     logi_location:get_application(Location), logi_location:get_module(Location)),
    case Sinks of
        [] -> Logger0;
        _  ->
            Headers = merge(Logger0#?LOGGER.headers, proplists:get_value(headers, Options, none)),
            Metadata = merge(Logger0#?LOGGER.metadata, proplists:get_value(metadata, Options, none)),
            Context = logi_context:new(os:timestamp(), Severity, Location, Headers, Metadata), % TODO: reuse the timestamp
            case apply_filters(Context, Options, Logger0) of
                {false, Logger1} -> {[],    Context, Logger1};
                {true,  Logger1} -> {Sinks, Context, Logger1}
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
-spec apply_filters(logi_context:context(), logi_filter:options(), logger()) -> {boolean(), logger()}.
apply_filters(Context, Options, Logger) ->
    apply_filters(Logger#?LOGGER.filters, Context, Options, Logger, []).

%% TODO: 適用順序保証が必要かどうか
-spec apply_filters([logi_filter:filter()], logi_context:context(), logi_filter:options(), logger(), [logi_filter:filter()]) ->
                           {boolean(), logger()}.
apply_filters([], _Context, _Options, Logger, Acc) ->
    {true, Logger#?LOGGER{filters = lists:reverse(Acc)}};
apply_filters([F0 | Filters], Context, Options, Logger, Acc) ->
    case logi_filter:apply(Context, Options, F0) of
        {true,  F1} -> apply_filters(Filters, Context, Options, Logger, [F1 | Acc]);
        {false, F1} -> {false, Logger#?LOGGER{filters = lists:reverse([F1 | Acc], Filters)}}
    end.

-spec get_and_validate(atom(), list(), term(), fun ((term()) -> boolean())) -> term() | false.
get_and_validate(Key, List, Default, Validate) ->
    case proplists:get_value(Key, List, Default) of
        Default -> Default;
        Value   -> Validate(Value) andalso Value
    end.

-spec merge(none | #{}, none | #{}) -> none | #{}.
merge(none, Map)  -> Map;
merge(Map, none)  -> Map;
merge(Map0, Map1) -> maps:merge(Map0, Map1).
