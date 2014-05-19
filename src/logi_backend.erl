%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc バックエンドモジュールのインタフェース定義 および バックエンドオブジェクト操作関数を提供
-module(logi_backend).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%------------------------------------------------------------------------------------------------------------------------
-callback format(logi:backend(), io:format(), [term()], logi:format_options()) -> iodata().
-callback write(logi:backend(), iodata()) -> ok | {error, Reason::term()}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/4, make/5,
         is_backend/1,
         get_id/1,
         get_ref/1,
         get_module/1,
         get_condition_spec/1,
         get_options/1
        ]).

-export_type([
              backend/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-opaque backend() :: #logi_backend{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv make(Ref, Ref, Module, Conditions, Options)
-spec make(logi:backend_ref(), module(), logi:condition_spec(), logi:backend_options()) -> backend().
make(Ref, Module, ConditionSpec, Options) ->
    make(Ref, Ref, Module, ConditionSpec, Options).

%% @doc バックエンドオブジェクトを生成する
-spec make(logi:backend_id(), logi:backend_ref(), module(), logi:condition_spec(), logi:backend_options()) -> backend().
make(Id, Ref, Module, ConditionSpec, Options) ->
    case is_backend_ref(Ref) andalso is_atom(Module) andalso is_condition_spec(ConditionSpec) of
        false -> error(badarg, [Id, Ref, Module, ConditionSpec, Options]);
        true  ->
            #logi_backend{
               id        = Id,
               ref       = Ref,
               module    = Module,
               condition = ConditionSpec,
               options   = Options
              }
    end.

%% @doc 引数の値がbackend()型かどうかを判定する
-spec is_backend(backend() | term()) -> boolean().
is_backend(X) -> is_record(X, logi_backend).

%% @doc バックエンドのIDを取得する
-spec get_id(backend()) ->  logi:backend_id().
get_id(#logi_backend{id = Id}) -> Id.

%% @doc バックエンドプロセスへの参照を取得する
-spec get_ref(backend()) -> logi:backend_ref().
get_ref(#logi_backend{ref = Ref}) -> Ref.

%% @doc バックエンドのモジュールを取得する
-spec get_module(backend()) -> module().
get_module(#logi_backend{module = Module}) -> Module.

%% @doc バックエンドがログ出力を担当する際の条件指定を取得する
-spec get_condition_spec(backend()) -> logi:condition_spec().
get_condition_spec(#logi_backend{condition = ConditionSpec}) -> ConditionSpec.

%% @doc バックエンドに指定されているオプションを取得する
-spec get_options(backend()) -> logi:backend_options().
get_options(#logi_backend{options = Options}) -> Options.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_backend_ref(logi:backend_ref() | term()) -> boolean().
is_backend_ref(Ref) -> is_atom(Ref) orelse is_pid(Ref).

-spec is_condition_spec(logi:condition_spec()) -> boolean().
is_condition_spec(Level) when is_atom(Level) -> logi:is_log_level(Level);
is_condition_spec({Level, Condition})        -> logi:is_log_level(Level) andalso is_condition(Condition);
is_condition_spec(List) when is_list(List)   ->
    lists:all(fun (X) -> (not is_list(X)) andalso is_condition_spec(X) end, List);
is_condition_spec(_) -> false.

-spec is_condition(logi:condition()) -> boolean().
is_condition(always)             -> true;
is_condition({match, {M, F, _}}) -> is_atom(M) andalso is_atom(F);
is_condition(_)                  -> false.
