%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_context).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([new/6]).
-export([is_context/1]).
-export([to_map/1]).
-export([get_channel/1]).
-export([get_timestamp/1]).
-export([get_severity/1]).
-export([get_location/1]).
-export([get_headers/1]).
-export([get_metadata/1]).

-export_type([context/0, context_map/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(CONTEXT, ?MODULE).

-record(?CONTEXT,
        {
          channel   :: logi_channel:id(),
          timestamp :: erlang:timestamp(),
          severity  :: logi:severity(),
          location  :: logi_location:location(),
          headers   :: logi:headers(),
          metadata  :: logi:metadata()
        }).

-opaque context() :: #?CONTEXT{}.

-type context_map() ::
        #{
           channel   => logi_channel:id(),
           timestamp => erlang:timestamp(),
           severity  => logi:severity(),
           location  => logi_location:location(),
           headers   => logi:headers(),
           metadata  => logi:metadata()
         }.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec new(logi_channel:id(), erlang:timestamp(), logi:severity(), logi_location:location(), logi:headers(), logi:metadata()) ->
                 context().
new(Channel, Timestamp, Severity, Location, Headers, Metadata) ->
    #?CONTEXT{
        channel = Channel,
        timestamp = Timestamp,
        severity = Severity,
        location = Location,
        headers = Headers,
        metadata = Metadata
       }.

-spec is_context(context() | term()) -> boolean().
is_context(X) -> is_record(X, ?CONTEXT).

-spec to_map(context()) -> context_map().
to_map(C) ->
    #{
       channel   => C#?CONTEXT.channel,
       timestamp => C#?CONTEXT.timestamp,
       severity  => C#?CONTEXT.severity,
       location  => C#?CONTEXT.location,
       headers   => C#?CONTEXT.headers,
       metadata  => C#?CONTEXT.metadata
     }.

-spec get_channel(context()) -> logi_channel:id().
get_channel(#?CONTEXT{channel = Channel}) -> Channel.

-spec get_timestamp(context()) -> erlang:timestamp().
get_timestamp(#?CONTEXT{timestamp = Timestamp}) -> Timestamp.

-spec get_severity(context()) -> logi:severity().
get_severity(#?CONTEXT{severity = Severity}) -> Severity.

-spec get_location(context()) -> logi_location:location().
get_location(#?CONTEXT{location = Location}) -> Location.

-spec get_headers(context()) -> logi:headers().
get_headers(#?CONTEXT{headers = Headers}) -> Headers.

-spec get_metadata(context()) -> logi:metadata().
get_metadata(#?CONTEXT{metadata = Metadata}) -> Metadata.
