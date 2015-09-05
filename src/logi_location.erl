%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Message issued location
-module(logi_location).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         guess/0,
         new/3, new/6,
         get_node/1,
         get_process/1,
         get_application/1,
         get_module/1,
         get_function/1,
         get_line/1,
         to_map/1,
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
%% TODO:
-spec guess() -> location().
guess() ->
    case process_info(self(), current_stacktrace) of % XXX: 末尾呼び出しやHiPEだと不正確な結果になる
        {current_stacktrace, [_, _, {Module, Function, _, Location} | _]} ->
            Line = proplists:get_value(line, Location, 0),
            new(Module, Function, Line);
        _ ->
            new(undefined, undefined, 0)
    end.

%% @equiv new(node(), self(), guess_application(Module), Module, Function, Line)
-spec new(module(), atom(), line()) -> location().
new(Module, Function, Line) ->
    new(node(), self(), guess_application(Module), Module, Function, Line).

%% @doc Creates a new location object
-spec new(node(), pid(), atom(), module(), atom(), line()) -> location().
new(Node, Pid, Application, Module, Function, Line) ->
    #logi_location{
       node        = Node,
       process     = Pid,
       application = Application,
       module      = Module,
       function    = Function,
       line        = Line
      }.

%% @doc Gets the node name of `Location'
-spec get_node(Location :: location()) -> node().
get_node(#logi_location{node = Node}) -> Node.

%% @doc Gets the PID of `Location'
-spec get_process(Location :: location()) -> pid().
get_process(#logi_location{process = Pid}) -> Pid.

%% @doc Gets the application of `Location'
-spec get_application(Location :: location()) -> atom().
get_application(#logi_location{application = App}) -> App.

%% @doc Gets the module of `Location'
-spec get_module(Location :: location()) -> module().
get_module(#logi_location{module = Module}) -> Module.

%% @doc Gets the function of `Location'
-spec get_function(Location :: location()) -> atom().
get_function(#logi_location{function = Function}) -> Function.

%% @doc Gets the line of `Location'
-spec get_line(Location :: location()) -> line().
get_line(#logi_location{line = Line}) -> Line.

%% @doc Converts `Location' to a map
-spec to_map(Location :: location()) -> Map when
      Map :: #{
        node        => node(),
        process     => pid(),
        application => atom(),
        module      => module(),
        function    => atom(),
        line        => line()
       }.
to_map(L) ->
    #{
       node        => L#logi_location.node,
       process     => L#logi_location.process,
       application => L#logi_location.application,
       module      => L#logi_location.module,
       function    => L#logi_location.function,
       line        => L#logi_location.line
     }.

%% @doc Guesses the application to which `Module' belongs
-spec guess_application(module()) -> atom() | undefined.
guess_application(Module) ->
    case application:get_application(Module) of
        {ok, App} -> App;
        undefined -> undefined
    end.
