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

-export_type([logger/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(LOGGER, ?MODULE).

-record(?LOGGER,
        {
          channel_id                  :: logi_channel:id(),
          headers              = none :: none | logi:headers(),
          metadata             = none :: none | logi:headers(),
          context_handler      = none :: none | logi:context_handler(),
          frequency_controller = none :: none | logi:frequency_controller()
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
    ContextHandler = get_and_validate(context_handler, Options, none, fun is_context_handler/1),
    ContextHandler =:= false andalso error(badarg, Args),

    Logger =
        #?LOGGER{
            channel_id      = ChannelId,
            headers         = Headers, % TODO: #{}ならnoneにする
            metadata        = Metadata,
            context_handler = ContextHandler
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
           context_handler      => L#?LOGGER.context_handler,
           frequency_controller => L#?LOGGER.frequency_controller
         })).

-spec from_map(logi:logger_map()) -> logger().
from_map(#{channel_id := ChannelId} = Map) ->
    new(ChannelId,
        [
         {headers, maps:get(headers, Map, none)},
         {metadata, maps:get(metadata, Map, none)},
         {context_handler, maps:get(context_handler, Map, none)},
         {frequency_controller, maps:get(frequency_controller, Map, none)}
        ]);
from_map(Map) ->
    error(badarg, [Map]).

-spec set_headers(logi:headers(), ignore | overwrite | supersede, logger()) -> logger().
set_headers(Headers, ignore,    Logger) -> Logger#?LOGGER{headers = maps:merge(Headers, Logger#?LOGGER.headers)};
set_headers(Headers, overwrite, Logger) -> Logger#?LOGGER{headers = maps:merge(Logger#?LOGGER.headers, Headers)};
set_headers(Headers, supersede, Logger) -> Logger#?LOGGER{headers = Headers}.

-spec delete_headers([term()], logger()) -> logger().
delete_headers(Keys, Logger) ->
    Logger#?LOGGER{headers = maps:without(Keys, Logger#?LOGGER.headers)}.

-spec set_metadata(logi:metadata(), ignore | overwrite | supersede, logger()) -> logger().
set_metadata(Metadata, ignore,    Logger) -> Logger#?LOGGER{metadata = maps:merge(Metadata, Logger#?LOGGER.metadata)};
set_metadata(Metadata, overwrite, Logger) -> Logger#?LOGGER{metadata = maps:merge(Logger#?LOGGER.metadata, Metadata)};
set_metadata(Metadata, supersede, Logger) -> Logger#?LOGGER{metadata = Metadata}.

-spec delete_metadata([term()], logger()) -> logger().
delete_metadata(Keys, Logger) ->
    Logger#?LOGGER{metadata = maps:without(Keys, Logger#?LOGGER.metadata)}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec get_and_validate(atom(), list(), term(), fun ((term()) -> boolean())) -> term() | false.
get_and_validate(Key, List, Default, Validate) ->
    case proplists:get_value(Key, List, Default) of
        Default -> Default;
        Value   -> Validate(Value) andalso Value
    end.

-spec is_context_handler({module(), term()} | term()) -> boolean().
is_context_handler({M, _}) -> is_atom(M);
is_context_handler(_)      -> false.
