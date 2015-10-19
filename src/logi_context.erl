%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Message Context
-module(logi_context).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/7, unsafe_new/7]).
-export([is_context/1]).
-export([to_map/1, from_map/1]).
-export([get_channel/1, get_timestamp/1, get_severity/1, get_subject/1, get_location/1, get_headers/1, get_metadata/1]).

-export_type([context/0]).
-export_type([map_form/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(CONTEXT, ?MODULE).

-record(?CONTEXT,
        {
          channel   :: logi_channel:id(),
          timestamp :: erlang:timestamp(),
          severity  :: logi:severity(),
          subject   :: term(),
          location  :: logi_location:location(),
          headers   :: logi:headers(),
          metadata  :: logi:metadata()
        }).

-opaque context() :: #?CONTEXT{}.
%% A context

-type map_form() ::
        #{
           channel   => logi_channel:id(),
           timestamp => erlang:timestamp(),
           severity  => logi:severity(),
           subject   => term(),
           location  => logi_location:location(),
           headers   => logi:headers(),
           metadata  => logi:metadata()
         }.
%% The map representation of a context

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Channel, os:timestamp(), Severity, undefined, logi_location:guess_location(), #{}, #{})
-spec new(logi_channel:id(), logi:severity()) -> context().
new(Channel, Severity) -> new(Channel, os:timestamp(), Severity, undefined, logi_location:guess_location(), #{}, #{}).

%% @doc Creates a new context object
-spec new(logi_channel:id(), erlang:timestamp(), logi:severity(), term(), logi_location:location(), logi:headers(),
          logi:metadata()) -> context().
new(Channel, Timestamp, Severity, Subject, Location, Headers, Metadata) ->
    Args = [Channel, Timestamp, Severity, Subject, Location, Headers, Metadata],
    _ = is_atom(Channel) orelse error(badarg, Args),
    _ = logi_utils:is_timestamp(Timestamp) orelse error(badarg, Args),
    _ = logi:is_severity(Severity) orelse error(badarg, Args),
    _ = logi_location:is_location(Location) orelse error(badarg, Args),
    _ = is_map(Headers) orelse error(badarg, Args),
    _ = is_map(Metadata) orelse error(badarg, Args),
    unsafe_new(Channel, Timestamp, Severity, Subject, Location, Headers, Metadata).

%% @doc Equivalent to {@link new/6} except omission of the arguments validation
-spec unsafe_new(logi_channel:id(), erlang:timestamp(), logi:severity(), term(), logi_location:location(), logi:headers(),
                 logi:metadata()) -> context().
unsafe_new(Channel, Timestamp, Severity, Subject, Location, Headers, Metadata) ->
    #?CONTEXT{
        channel   = Channel,
        timestamp = Timestamp,
        severity  = Severity,
        subject   = Subject,
        location  = Location,
        headers   = Headers,
        metadata  = Metadata
       }.

%% @doc Returns `true' if `X' is a context object, `false' otherwise.
-spec is_context(X :: (context() | term())) -> boolean().
is_context(X) -> is_record(X, ?CONTEXT).

%% @doc Creates a new context object from `Map'
-spec from_map(map_form()) -> context().
from_map(Map) ->
    _ = is_map(Map) orelse error(badarg, [Map]),
    new(maps:get(channel, Map),
        maps:get(timestamp, Map),
        maps:get(severity, Map),
        maps:get(subject, Map),
        maps:get(location, Map),
        maps:get(headers, Map),
        maps:get(metadata, Map)).

%% @doc Converts `Context' into a map form
-spec to_map(context()) -> map_form().
to_map(C) ->
    #{
       channel   => C#?CONTEXT.channel,
       timestamp => C#?CONTEXT.timestamp,
       severity  => C#?CONTEXT.severity,
       subject   => C#?CONTEXT.subject,
       location  => C#?CONTEXT.location,
       headers   => C#?CONTEXT.headers,
       metadata  => C#?CONTEXT.metadata
     }.

%% @doc Gets the channel of `Context'
-spec get_channel(Context :: context()) -> logi_channel:id().
get_channel(#?CONTEXT{channel = Channel}) -> Channel.

%% @doc Gets the timestamp of `Context'
-spec get_timestamp(Context :: context()) -> erlang:timestamp().
get_timestamp(#?CONTEXT{timestamp = Timestamp}) -> Timestamp.

%% @doc Gets the severity of `Context'
-spec get_severity(Context :: context()) -> logi:severity().
get_severity(#?CONTEXT{severity = Severity}) -> Severity.

%% @doc Gets the subject of `Context'
-spec get_subject(Context :: context()) -> term().
get_subject(#?CONTEXT{subject = Subject}) -> Subject.

%% @doc Gets the location of `Context'
-spec get_location(Context :: context()) -> logi_location:location().
get_location(#?CONTEXT{location = Location}) -> Location.

%% @doc Gets the headers of `Context'
-spec get_headers(Context :: context()) -> logi:headers().
get_headers(#?CONTEXT{headers = Headers}) -> Headers.

%% @doc Gets the metadata of `Context'
-spec get_metadata(Context :: context()) -> logi:metadata().
get_metadata(#?CONTEXT{metadata = Metadata}) -> Metadata.
