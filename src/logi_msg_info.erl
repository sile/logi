%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログメッセージの付加情報を取得する
-module(logi_msg_info).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/5,
         get_severity/1,
         get_timestamp/1,
         get_headers/1,
         get_metadata/1,
         get_omitted_count/1
        ]).

-export_type([
              info/0
             ]).

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
%% @doc メッセージ付加情報を生成する
%%
%% 引数として与えた各値の型が正しいことを担保するのは、呼び出し側の責任
%%
%% @private
-spec make(logi:severity(), erlang:timestamp(), logi:headers(), logi:metadata(), non_neg_integer()) -> info().
make(Severity, Timestamp, Headers, MetaData, OmittedCount) ->
    #logi_msg_info{
       severity      = Severity,
       timestamp     = Timestamp,
       headers       = Headers,
       metadata      = MetaData,
       omitted_count = OmittedCount
      }.

-spec get_severity(info()) -> logi:severity().
get_severity(#logi_msg_info{severity = Severity}) -> Severity.

-spec get_timestamp(info()) -> erlang:timestamp().
get_timestamp(#logi_msg_info{timestamp = Timestamp}) -> Timestamp.

-spec get_headers(info()) -> logi:headers().
get_headers(#logi_msg_info{headers = Headers}) -> Headers.

-spec get_metadata(info()) -> logi:metadata().
get_metadata(#logi_msg_info{metadata = MetaData}) -> MetaData.

-spec get_omitted_count(info()) -> non_neg_integer().
get_omitted_count(#logi_msg_info{omitted_count = Count}) -> Count.
