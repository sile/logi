%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc バックエンドモジュールのインタフェース定義 および バックエンドオブジェクト操作関数を提供
-module(logi_backend).

%%------------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%------------------------------------------------------------------------------------------------------------------------
-callback write(logi_backend:backend(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> any().

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/1, make/4,
         is_backend/1,
         get_id/1,
         get_process/1,
         get_module/1,
         get_data/1
        ]).

-export_type([
              backend/0,
              spec/0,
              id/0,
              process/0,
              data/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(BACKEND, ?MODULE).

-record(?BACKEND,
        {
          id      :: id(),
          process :: process(),
          module  :: module(),
          data    :: data()
        }).

-opaque backend() :: #?BACKEND{}.

-type spec() :: {process(), module(), data()}
              | {id(), process(), module(), data()}.

-type id()      :: term().
-type process() :: atom().
-type data()    :: term().

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc バックエンドオブジェクトを生成する
-spec make(id(), process(), module(), data()) -> backend().
make(Id, Process, Module, Data) ->
    case is_atom(Process) andalso is_atom(Module) of
        false -> error(badarg, [Id, Process, Module, Data]);
        true  ->
            #?BACKEND{
                id      = Id,
                process = Process,
                module  = Module,
                data    = Data
               }
    end.

%% @doc spec()をもとにbackend()を生成する
-spec make(spec()) -> backend().
make({Process, Module, Data})     -> make(Process, Process, Module, Data);
make({Id, Process, Module, Data}) -> make(Id, Process, Module, Data);
make(Arg)                         -> error(badrag, [Arg]).

%% @doc 引数の値がbackend()型かどうかを判定する
-spec is_backend(backend() | term()) -> boolean().
is_backend(X) -> is_record(X, ?BACKEND).

%% @doc バックエンドのIDを取得する
-spec get_id(backend()) ->  id().
get_id(#?BACKEND{id = Id}) -> Id.

%% @doc バックエンドプロセスを取得する
-spec get_process(backend()) -> process().
get_process(#?BACKEND{process = Process}) -> Process.

%% @doc バックエンドのモジュールを取得する
-spec get_module(backend()) -> module().
get_module(#?BACKEND{module = Module}) -> Module.

%% @doc バックエンドに紐付く任意データを取得する
-spec get_data(backend()) -> data().
get_data(#?BACKEND{data = Data}) -> Data.
