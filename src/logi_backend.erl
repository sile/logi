%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc バックエンドモジュールのインタフェース定義 および バックエンドオブジェクト操作関数を提供
-module(logi_backend).

%%------------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%------------------------------------------------------------------------------------------------------------------------
-callback format(logi:backend(), io:format(), [term()], logi:msg_info()) -> iodata().
-callback write(logi:backend(), iodata()) -> ok | {error, Reason::term()}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/4, make/5,
         update/2,
         is_backend/1,
         get_id/1,
         get_ref/1,
         get_module/1,
         get_condition/1,
         get_options/1
        ]).

-export_type([
              backend/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(BACKEND, ?MODULE).

-record(?BACKEND,
        {
          id        :: logi:backend_id(),
          ref       :: logi:backend_ref(),
          module    :: module(),
          condition :: logi:condition(),
          options   :: logi:backend_options()
        }).

-opaque backend() :: #?BACKEND{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv make(Ref, Ref, Module, Conditions, Options)
-spec make(logi:backend_ref(), module(), logi:condition(), logi:backend_options()) -> backend().
make(Ref, Module, Condition, Options) ->
    make(Ref, Ref, Module, Condition, Options).

%% @doc バックエンドオブジェクトを生成する
-spec make(logi:backend_id(), logi:backend_ref(), module(), logi:condition(), logi:backend_options()) -> backend().
make(Id, Ref, Module, Condition, Options) ->
    case is_backend_ref(Ref) andalso is_atom(Module) andalso logi_condition:is_condition(Condition) of
        false -> error(badarg, [Id, Ref, Module, Condition, Options]);
        true  ->
            #?BACKEND{
                id        = Id,
                ref       = Ref,
                module    = Module,
                condition = Condition,
                options   = Options
               }
    end.

%% @doc バックエンドオブジェクトを更新する
-spec update(UpdateList, backend()) -> backend() when
      UpdateList  :: [UpdateEntry],
      UpdateEntry :: {id, logi:backend_id()} | {ref, logi:backend_ref()} | {module, module()}
                   | {options, logi:backend_options()} | {condition, logi:condition()}.
update(UpdateList, #?BACKEND{} = Backend) when is_list(UpdateList) ->
    make(logi_util_assoc:fetch(id, UpdateList, Backend#?BACKEND.id),
         logi_util_assoc:fetch(ref, UpdateList, Backend#?BACKEND.ref),
         logi_util_assoc:fetch(module, UpdateList, Backend#?BACKEND.module),
         logi_util_assoc:fetch(condition, UpdateList, Backend#?BACKEND.condition),
         logi_util_assoc:fetch(options, UpdateList, Backend#?BACKEND.options));
update(UpdateList, Backend) -> error(badarg, [UpdateList, Backend]).

%% @doc 引数の値がbackend()型かどうかを判定する
-spec is_backend(backend() | term()) -> boolean().
is_backend(X) -> is_record(X, ?BACKEND).

%% @doc バックエンドのIDを取得する
-spec get_id(backend()) ->  logi:backend_id().
get_id(#?BACKEND{id = Id}) -> Id.

%% @doc バックエンドプロセスへの参照を取得する
-spec get_ref(backend()) -> logi:backend_ref().
get_ref(#?BACKEND{ref = Ref}) -> Ref.

%% @doc バックエンドのモジュールを取得する
-spec get_module(backend()) -> module().
get_module(#?BACKEND{module = Module}) -> Module.

%% @doc バックエンドがログ出力を担当する際の条件指定を取得する
-spec get_condition(backend()) -> logi:condition().
get_condition(#?BACKEND{condition = Condition}) -> Condition.

%% @doc バックエンドに指定されているオプションを取得する
-spec get_options(backend()) -> logi:backend_options().
get_options(#?BACKEND{options = Options}) -> Options.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_backend_ref(logi:backend_ref() | term()) -> boolean().
is_backend_ref(Ref) -> is_atom(Ref) orelse is_pid(Ref).
