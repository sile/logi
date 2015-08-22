%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The additional informations on a log message
-module(logi_msg_info).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/5]).
-export([get_severity/1, get_timestamp/1, get_headers/1, get_metadata/1, get_omitted_count/1]).

-export_type([info/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(logi_msg_info,
        {
          severity      :: logi:severity(),
          timestamp     :: erlang:timestamp(),
          headers       :: logi:headers(),
          metadata      :: logi:metadata(),
          omitted_count :: non_neg_integer()
        }).

-opaque info() :: #logi_msg_info{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Function
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Makes a `info()' object
-spec make(logi:severity(), erlang:timestamp(), logi:headers(), logi:metadata(), non_neg_integer()) -> info().
make(Severity, Timestamp, Headers, MetaData, OmittedCount) ->
    #logi_msg_info{
       severity      = Severity,
       timestamp     = Timestamp,
       headers       = Headers,
       metadata      = MetaData,
       omitted_count = OmittedCount
      }.

%% Gets the severity
-spec get_severity(info()) -> logi:severity().
get_severity(#logi_msg_info{severity = Severity}) -> Severity.

%% Gets the timestamp
-spec get_timestamp(info()) -> erlang:timestamp().
get_timestamp(#logi_msg_info{timestamp = Timestamp}) -> Timestamp.

%% Gets the headers
-spec get_headers(info()) -> logi:headers().
get_headers(#logi_msg_info{headers = Headers}) -> Headers.

%% Gets the metadata
-spec get_metadata(info()) -> logi:metadata().
get_metadata(#logi_msg_info{metadata = MetaData}) -> MetaData.

%% Gets the omitted count
-spec get_omitted_count(info()) -> non_neg_integer().
get_omitted_count(#logi_msg_info{omitted_count = Count}) -> Count.
