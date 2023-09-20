%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The location where log message issued
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > Location = logi_location:new(lists, filter, 10).
%% > logi_location:to_map(Location).
%% #{application => stdlib,
%%   function => filter,
%%   line => 10,
%%   module => lists,
%%   process => &lt;0.91.0&gt;}
%% </pre>
%%
%% {@link guess_location/0} returns the current location.
%% <pre lang="erlang">
%% > Location = logi_location:guess_location(). % If `logi_transform' is not used, a warning will be emitted.
%% =WARNING REPORT==== 19-Oct-2015::14:02:26 ===
%%    pid: &lt;0.91.0&gt;
%%    module: erl_eval
%%    function: do_apply
%%    line: 673
%%    msg: "A deprecated function 'logi_location:guess_location/0' is called. Please use the `{parse_transform, logi_transform}' compiler option."
%%
%% > logi_location:to_map(Location).
%% #{application => stdlib,
%%   function => do_apply,
%%   line => 673,
%%   module => erl_eval,
%%   process => &lt;0.91.0&gt;}
%% </pre>
%% @end
-module(logi_location).

-deprecated({guess_location, 0}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/3, new/5, unsafe_new/5]).
-export([is_location/1]).
-export([to_map/1, from_map/1]).
-export([guess_location/0]).
-export([guess_application/1]).
-export([get_process/1, get_application/1, get_module/1, get_function/1, get_line/1]).

-export_type([location/0]).
-export_type([map_form/0]).
-export_type([application/0, line/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(LOCATION, ?MODULE).

-record(?LOCATION,
        {
          process     :: pid(),
          application :: application(),
          module      :: module(),
          function    :: atom(),
          line        :: line()
        }).

-opaque location() :: #?LOCATION{}.
%% A log message issued location

-type map_form() ::
        #{
           process     => pid(),
           application => application(),
           module      => module(),
           function    => atom(),
           line        => line()
         }.
%% The map representation of a location

-type application() :: atom().
%% An application name

-type line() :: pos_integer() | 0.
%% A line number
%%
%% `0' means "Unknown Line"

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(self(), guess_application(Module), Module, Function, Line)
-spec new(module(), atom(), line()) -> location().
new(Module, Function, Line) ->
    new(self(), guess_application(Module), Module, Function, Line).

%% @doc Creates a new location object
-spec new(pid(), application(), module(), atom(), line()) -> location().
new(Pid, Application, Module, Function, Line) ->
    Args = [Pid, Application, Module, Function, Line],
    _ = is_pid(Pid) orelse error(badarg, Args),
    _ = is_atom(Application) orelse error(badarg, Args),
    _ = is_atom(Module) orelse error(badarg, Args),
    _ = is_atom(Function) orelse error(badarg, Args),
    _ = (is_integer(Line) andalso Line >= 0) orelse error(badarg, Args),
    unsafe_new(Pid, Application, Module, Function, Line).

%% @doc Equivalent to {@link new/5} except omission of the arguments validation
-spec unsafe_new(pid(), application(), module(), atom(), line()) -> location().
unsafe_new(Pid, Application, Module, Function, Line) ->
    #?LOCATION{
        process     = Pid,
        application = Application,
        module      = Module,
        function    = Function,
        line        = Line
       }.

%% @doc Returns `true' if `X' is a location object, `false' otherwise.
-spec is_location(X :: (location() | term())) -> boolean().
is_location(X) -> is_record(X, ?LOCATION).

%% @doc Creates a new location from `Map'
%%
%% Default Value:
%% - process: `self()'
%% - application: `guess_application(maps:get(module, Map))'
%% - module: `undefined'
%% - function: `undefined'
%% - line: `0'
-spec from_map(Map :: map_form()) -> location().
from_map(Map) ->
    _ = is_map(Map) orelse error(badarg, [Map]),
    Module = maps:get(module, Map, undefined),
    Application =
        case maps:find(application, Map) of
            error     -> guess_application(Module);
            {ok, App} -> App
        end,
    new(maps:get(process, Map, self()),
        Application,
        Module,
        maps:get(function, Map, undefined),
        maps:get(line, Map, 0)).

%% @doc Converts `Location' into a map form
-spec to_map(Location :: location()) -> map_form().
to_map(L) ->
    #{
       process     => L#?LOCATION.process,
       application => L#?LOCATION.application,
       module      => L#?LOCATION.module,
       function    => L#?LOCATION.function,
       line        => L#?LOCATION.line
     }.

%% @doc Guesses the location where the function is called (parse transformation fallback)
%%
%% This function is too slow and provided for debugging/testing purposes only.
%%
%% @deprecated Please use the `{parse_transform, logi_transform}' compiler option
%%             which replaces the function call to a more efficient code.
-spec guess_location() -> location().
guess_location() ->
    Location =
        case process_info(self(), current_stacktrace) of % NOTE: In the case of tail calls, this will return inaccurate result
            {current_stacktrace, Stack} ->
                case lists:dropwhile(fun ({M, _, _, _}) -> guess_application(M) =:= logi end, Stack) of
                    [{Module, Function, _, Loc} | _] ->
                        Line = proplists:get_value(line, Loc, 0),
                        new(Module, Function, Line);
                    _ -> new(undefined, undefined, 0)
                end;
            _ -> new(undefined, undefined, 0)
        end,
    _ = case application:get_env(logi, warn_no_parse_transform, true) of
            false -> ok;
            true  ->
                Message =
                    "A deprecated function 'logi_location:guess_location/0' is called. "
                    "Please use the `{parse_transform, logi_transform}' compiler option.",
                case application:get_env(logi, warnings_as_errors, false) of
                    true  -> error(Message);
                    false ->
                        error_logger:warning_report(
                          [
                           {pid, get_process(Location)},
                           {module, get_module(Location)},
                           {function, get_function(Location)},
                           {line, get_line(Location)},
                           {msg, Message}
                          ])
                end
        end,
    Location.

%% @doc Guesses the application to which `Module' belongs
-spec guess_application(module()) -> atom() | undefined.
guess_application(Module) ->
    case application:get_application(Module) of
        {ok, App} -> App;
        undefined -> undefined
    end.

%% @doc Gets the PID of `Location'
-spec get_process(Location :: location()) -> pid().
get_process(#?LOCATION{process = Pid}) -> Pid.

%% @doc Gets the application of `Location'
-spec get_application(Location :: location()) -> atom().
get_application(#?LOCATION{application = App}) -> App.

%% @doc Gets the module of `Location'
-spec get_module(Location :: location()) -> module().
get_module(#?LOCATION{module = Module}) -> Module.

%% @doc Gets the function of `Location'
-spec get_function(Location :: location()) -> atom().
get_function(#?LOCATION{function = Function}) -> Function.

%% @doc Gets the line of `Location'
-spec get_line(Location :: location()) -> line().
get_line(#?LOCATION{line = Line}) -> Line.
