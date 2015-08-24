%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The additional informations on a log message
%%
%% TODO(?): Renames to logi_message
-module(logi_msg_info).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/5]).
-export([get_severity/1, get_timestamp/1, get_headers/1, get_metadata/1, get_location/1]).

-export_type([info/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(logi_msg_info,
        {
          severity  :: logi:severity(),
          timestamp :: erlang:timestamp(),
          location  :: logi_location:location(),
          headers   :: logi:headers(),
          metadata  :: logi:metadata()
        }).

-opaque info() :: #logi_msg_info{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Function
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Makes a `info()' object
-spec make(logi:severity(), erlang:timestamp(), logi_location:location(), logi:headers(), logi:metadata()) -> info().
make(Severity, Timestamp, Location, Headers, MetaData) ->
    #logi_msg_info{
       severity  = Severity,
       timestamp = Timestamp,
       location  = Location,
       headers   = Headers,
       metadata  = MetaData
      }.

%% @doc Gets the severity of the message
-spec get_severity(info()) -> logi:severity().
get_severity(#logi_msg_info{severity = Severity}) -> Severity.

%% @doc Gets the timestamp of the message
-spec get_timestamp(info()) -> erlang:timestamp().
get_timestamp(#logi_msg_info{timestamp = Timestamp}) -> Timestamp.

%% @doc Gets the location of the message
-spec get_location(info()) -> logi_location:location().
get_location(#logi_msg_info{location = Location}) -> Location.

%% @doc Gets the headers of the message
-spec get_headers(info()) -> logi:headers().
get_headers(#logi_msg_info{headers = Headers}) -> Headers.

%% @doc Gets the metadata of the message
-spec get_metadata(info()) -> logi:metadata().
get_metadata(#logi_msg_info{metadata = MetaData}) -> MetaData.
