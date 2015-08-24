%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログ出力用のコンテキスト情報を保持するためのモジュール
%% @private
-module(logi_context).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/1, make/3,
         is_context/1,
         get_full_metadata/2,
         get_full_headers/2,
         is_output_allowed/3,
         get_logger/1,
         set_headers/2,
         get_headers/1,
         set_metadata/2,
         get_metadata/1
        ]).

-export([apply_severity_mapper/3]).

-export_type([
              context/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(logi_context,
        {
          logger    :: logi:logger(),
          headers   :: logi:headers(),
          metadata  :: logi:metadata(),
          frequency :: undefined | logi_frequency:controller(),
          severity_mapper :: undefined | severity_mapper()
        }).

-opaque context() :: #logi_context{}.

-type severity_mapper() :: {severity_map_fun(), severity_mapper_state()}.
-type severity_mapper_state() :: term().
-type severity_map_fun() :: fun ((logi:logger(), logi:severity(), logi:location(), logi:headers(), logi:metadata(),
                                  severity_mapper_state()) -> logi:severity() |
                                                              {logi:severity(), severity_mapper_state()}).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv make(LoggerId, [], [])
-spec make(logi:logger()) -> context().
make(LoggerId) ->
    make(LoggerId, [], []).

%% @doc コンテキストオブジェクトを生成する
-spec make(logi:logger(), logi:headers(), logi:metadata()) -> context().
make(LoggerId, Headers, MetaData) ->
    IsValid = is_atom(LoggerId) andalso logi_util_assoc:is_assoc_list(Headers) andalso logi_util_assoc:is_assoc_list(MetaData),
    case IsValid of
        false -> error(badarg, [LoggerId, Headers, MetaData]);
        true  ->
            #logi_context{
               logger   = LoggerId,
               headers  = lists:ukeysort(1, Headers),
               metadata = lists:ukeysort(1, MetaData)
              }
    end.

%% @doc 引数の値がコンテキストオブジェクトかどうかを判定する
-spec is_context(context() | term()) -> boolean().
is_context(X) -> is_record(X, logi_context).

%% @doc 完全なメタデータ情報を取得する
-spec get_full_metadata(logi:metadata(), logi:context()) -> logi:metadata().
get_full_metadata(LocalMetaData, Context) ->
    LocalMetaData ++ Context#logi_context.metadata.

%% @doc 完全なヘッダ情報を取得する
-spec get_full_headers(logi:headers(), logi:context()) -> logi:headers().
get_full_headers(LocalHeaders, Context) ->
    LocalHeaders ++ Context#logi_context.headers.

%% %% @doc 出力が許可されているかどうかを判定する
-spec is_output_allowed(logi:frequency_policy(), logi_msg_info:info(), context()) -> {boolean(), context()}.
is_output_allowed(Policy, MsgInfo, Context) ->
    {Controller0, Entries} =
        logi_frequency:flush_expired_entries(case Policy of
                                                 undefined -> 5;
                                                 _ -> maps:get(max_flush_count, Policy, 5)
                                             end,
                                             logi_msg_info:get_timestamp(MsgInfo),
                                             Context#logi_context.frequency),
    Context1 = Context#logi_context{frequency = Controller0},
    Context2 = lists:foldl(fun logi_frequency:output_overflow_message/2, Context1, Entries),
    {Result, Controller1} = logi_frequency:is_output_allowed(Policy, MsgInfo, Controller0),
    {Result, Context2#logi_context{frequency = Controller1}}.

%% @doc 対象となるロガーを取得する
-spec get_logger(context()) -> logi:logger().
get_logger(#logi_context{logger = Logger}) -> Logger.

%% 注意: Headersがソート済みであることは呼び出し元が保証する
-spec set_headers(logi:headers(), context()) -> context().
set_headers(Headers, Context) ->
    Context#logi_context{headers = Headers}.

-spec get_headers(context()) -> logi:headers().
get_headers(Context) ->
    Context#logi_context.headers.

%% 注意: MetaDataがソート済みであることは呼び出し元が保証する
-spec set_metadata(logi:metadata(), context()) -> context().
set_metadata(MetaData, Context) ->
    Context#logi_context{metadata = MetaData}.

-spec get_metadata(context()) -> logi:metadata().
get_metadata(Context) ->
    Context#logi_context.metadata.

%% TODO:
%% TODO: Supports skip
-spec apply_severity_mapper(logi:severity(), logi_location:location(), context()) -> {logi:severity(), context()}.
apply_severity_mapper(Severity, _Location, Context = #logi_context{severity_mapper = undefined}) ->
    {Severity, Context};
apply_severity_mapper(Severity0, Location, Context = #logi_context{severity_mapper = {MapFun, State0}}) ->
    #logi_context{logger = Logger, headers = Headers, metadata = MetaData} = Context,
    case MapFun(Logger, Severity0, Location, Headers, MetaData, State0) of
        {Severity, State1} -> {Severity, Context#logi_context{severity_mapper = {MapFun, State1}}};
        Severity           -> {Severity, Context}
    end.
