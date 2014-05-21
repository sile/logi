%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログ出力用のコンテキスト情報を保持するためのモジュール
%% @private
-module(logi_context).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/0, make/2,
         is_context/1,
         get_full_metadata/2,
         get_full_headers/2,
         is_output_allowed/3,
         set_headers/2,
         get_headers/1,
         set_metadata/2,
         get_metadata/1
        ]).

-export_type([
              context/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(logi_context,
        {
          headers              :: logi:headers(),
          metadata             :: logi:metadata(),
          frequency_controller :: logi_frequency_controller:controller()
        }).

-opaque context() :: #logi_context{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv make([], [])
-spec make() -> context().
make() ->
    make([], []).

%% @doc コンテキストオブジェクトを生成する
-spec make(logi:headers(), logi:metadata()) -> context().
make(Headers, MetaData) ->
    IsValid = logi_util_assoc:is_assoc_list(Headers) andalso logi_util_assoc:is_assoc_list(MetaData),
    case IsValid of
        false -> error(badarg, [Headers, MetaData]);
        true  ->
            #logi_context{
               headers  = lists:ukeysort(1, Headers),
               metadata = lists:ukeysort(1, MetaData),
               frequency_controller = logi_frequency_controller:make()
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

%% @doc 出力が許可されているかどうかを判定する
-spec is_output_allowed(logi:frequency_policy_spec(), logi:location(), context()) -> {{true, non_neg_integer()} | false, context()}.
is_output_allowed(Policy, Location, Context) ->
    {Result, Controller} =
        logi_frequency_controller:is_output_allowed(Policy, Location, Context#logi_context.frequency_controller),
    {Result, Context#logi_context{frequency_controller = Controller}}.

%% 注意: Headersがソート済みであることは呼び出し元が保証する
-spec set_headers(logi:headers(), context()) -> context().
set_headers(Headers, Context) ->
    Context#logi_context{headers = lists:keymerge(1, Headers, Context#logi_context.headers)}.

-spec get_headers(context()) -> logi:headers().
get_headers(Context) ->
    Context#logi_context.headers.

%% 注意: MetaDataがソート済みであることは呼び出し元が保証する
-spec set_metadata(logi:metadata(), context()) -> context().
set_metadata(MetaData, Context) ->
    Context#logi_context{metadata = lists:keymerge(1, MetaData, Context#logi_context.metadata)}.

-spec get_metadata(context()) -> logi:metadata().
get_metadata(Context) ->
    Context#logi_context.metadata.
