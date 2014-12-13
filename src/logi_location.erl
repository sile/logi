%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログ出力の位置情報を保持するためのモジュール
-module(logi_location).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/3, make/6,
         get_node/1,
         get_process/1,
         get_application/1,
         get_module/1,
         get_function/1,
         get_line/1,
         to_list/1,
         guess_application/1
        ]).

-export_type([
              location/0,
              line/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(logi_location,
        {
          node        :: node(),
          process     :: pid(),
          application :: atom(),
          module      :: module(),
          function    :: atom(),
          line        :: line()
        }).

-opaque location() :: #logi_location{}.

-type line() :: pos_integer() | 0. % 0 indicates 'Unknown Line'

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv make(node(), self(), guess_application(Module), Module, Function, Line)
-spec make(module(), atom(), line()) -> location().
make(Module, Function, Line) ->
    make(node(), self(), guess_application(Module), Module, Function, Line).

%% @doc 位置情報オブジェクトを作成する
-spec make(node(), pid(), atom(), module(), atom(), line()) -> location().
make(Node, Pid, Application, Module, Function, Line) ->
    #logi_location{
       node        = Node,
       process     = Pid,
       application = Application,
       module      = Module,
       function    = Function,
       line        = Line
      }.

%% @doc ノード名を取得する
-spec get_node(location()) -> node().
get_node(#logi_location{node = Node}) -> Node.

%% @doc プロセスIDを取得する
-spec get_process(location()) -> pid().
get_process(#logi_location{process = Pid}) -> Pid.

%% @doc アプリケーション名を取得する
-spec get_application(location()) -> atom().
get_application(#logi_location{application = App}) -> App.

%% @doc モジュール名を取得する
-spec get_module(location()) -> module().
get_module(#logi_location{module = Module}) -> Module.

%% @doc 関数名を取得する
-spec get_function(location()) -> atom().
get_function(#logi_location{function = Function}) -> Function.

%% @doc 行番号を取得する
-spec get_line(location()) -> line().
get_line(#logi_location{line = Line}) -> Line.

%% @doc 連想リスト形式に変換する
-spec to_list(location()) -> ordsets:ordset(Entry) when
      Entry :: {node, node()}
             | {process, pid()}
             | {application, atom()}
             | {module, module()}
             | {function, atom()}
             | {line, line()}.
to_list(Location) ->
    [{application, Location#logi_location.application},
     {function,    Location#logi_location.function},
     {line,        Location#logi_location.line},
     {module,      Location#logi_location.module},
     {node,        Location#logi_location.node},
     {process,     Location#logi_location.process}].

%% @doc モジュール名から、それが属するアプリケーションを推測する
-spec guess_application(module()) -> atom() | undefined.
guess_application(Module) ->
    case application:get_application(Module) of
        {ok, App} -> App;
        undefined -> undefined
    end.
