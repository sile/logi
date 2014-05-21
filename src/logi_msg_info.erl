%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログメッセージの付加情報を取得する
-module(logi_msg_info).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/5
        ]).

-export_type([
              info/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(logi_msg_info,
        {
          severity  :: logi:severity(),
          timestamp :: erlang:timestamp(),
          headers   :: logi:headers(),
          metadata  :: logi:metadata(),
          context   :: logi:context()
        }).

-opaque info() :: #logi_msg_info{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Function
%%------------------------------------------------------------------------------------------------------------------------
%% @doc メッセージ付加情報を生成する
%%
%% 引数として与えた各値の型が正しいことを担保するのは、呼び出し側の責任
-spec make(logi:severity(), erlang:timestamp(), logi:headers(), logi:metadata(), logi:context()) -> info().
make(Severity, Timestamp, Headers, MetaData, Context) ->
    #logi_msg_info{
       severity  = Severity,
       timestamp = Timestamp,
       headers   = Headers,
       metadata  = MetaData,
       context   = Context
      }.
