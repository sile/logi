%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sink object
%%
%% A sink will consume the log messages sent to the channel which the sink have been installed.
%%
%% ```
%% %%%
%% %%% Example
%% %%%
%% > ok = logi_channel:create(sample_log).
%% > Sink = logi_sink:new(logi_builtin_sink_null).
%% > {ok, _} = logi_channel:install_sink(sample_log, Sink).
%% > logi:info("Hello World", [], [{logger, sample_log}]). % `logi_builtin_sink_null:write/4' will be invoked
%% '''
-module(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2, new/3, new/4]).
-export([is_sink/1]).
-export([get_id/1, get_module/1, get_condition/1, get_extra_data/1]).
-export([get_normalized_condition/1]).
-export([to_map/1, from_map/1]).
-export([is_condition/1]).
-export([is_callback_module/1]).

-export_type([sink/0]).
-export_type([id/0]).
-export_type([callback_module/0]).
-export_type([condition/0, severity_condition/0, location_condition/0, normalized_condition/0]).
-export_type([extra_data/0]).
-export_type([map_form/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_context:context(), io:format(), logi:format_args(), extra_data()) -> any().

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(SINK, ?MODULE).

-record(?SINK,
        {
          id         :: id(),
          module     :: callback_module(),
          condition  :: condition(),
          extra_data :: extra_data()
        }).

-opaque sink() :: #?SINK{}.
%% A sink.

-type id() :: atom().
%% The identifier of a sink.
%% The sinks installed in the same channel must have different identifiers.

-type callback_module() :: module().
%% A module that implements the `sink' behaviour.

-type extra_data() :: term().
%% The value of the fourth arguemnt of the `write/4' callback function.
%%
%% NOTE: <br />
%% This value will be loaded from ETS every time the `write/4' is called.
%% Therefore, very huge data can cause a performance issue.

-type condition() :: severity_condition() | location_condition().
%% The condition to determine which messages to be consumed by a sink.

-type severity_condition() :: (Min :: logi:severity())
                            | {Min :: logi:severity(), Max :: logi:severity()}
                            | (Severities :: [logi:severity()]).
%% `Min': <br />
%% - The messages with `Min' or higher severity will be consumed. <br />
%%
%% `{Min, Max}': <br />
%% - The messages with severity between `Min' and `Max' will be consumed. <br />
%%
%% `Severities': <br />
%% - The messages with severity included in `Severities' will be consumed. <br />

-type location_condition() ::
        #{
           severity    => severity_condition(),
           application => logi:application() | [logi:application()],
           module      => module() | [module()]
         }.
%% The messages which satisfy `severity' (default is `debug') and are sent from the specified location will be consumed.
%%
%% The location is specified by `application' and `module' (OR condition).
%%
%% NOTE: The modules which does not belong to any application are forbidden.

-type normalized_condition() :: [logi:severity() |
                                 {logi:severity(), logi:application()} |
                                 {logi:severity(), logi:application(), module()}].
%% The normalized form of a `condition/0'.
%%
%% ```
%% > Normalize = fun (C) -> lists:sort(logi_sink:get_normalized_condition(logi_sink:new(null, logi_builtin_sink_null, C))) end.
%%
%% > Normalize(info).
%% [alert,critical,emergency,error,info,notice,warning]
%%
%% > Normalize({info, alert}).
%% [alert,critical,error,info,notice,warning]
%%
%% > Normalize(#{severity => [info], application => [kernel, stdlib]}).
%% [{info,kernel},{info,stdlib}]
%%
%% > Normalize(#{severity => [info], module => [lists, logi]}).
%% [{info,logi,logi},{info,stdlib,lists}]
%%
%% > Normalize(#{severity => [info], application => kernel, module => [lists, logi]}).
%% [{info,kernel},{info,logi,logi},{info,stdlib,lists}]
%% '''

-type map_form() ::
        #{
           id         => id(),
           module     => callback_module(),
           condition  => condition(),
           extra_data => extra_data()
         }.
%% The map form of a sink

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Module, Module)
-spec new(callback_module()) -> sink().
new(Module) -> new(Module, Module).

%% @equiv new(Id, Module, debug)
-spec new(id(), callback_module()) -> sink().
new(Id, Module) -> new(Id, Module, debug).

%% @equiv new(Id, Module, Condition, undefined)
-spec new(id(), callback_module(), condition()) -> sink().
new(Id, Module, Condition) -> new(Id, Module, Condition, undefined).

%% @doc Creates a new sink
-spec new(id(), callback_module(), condition(), extra_data()) -> sink().
new(Id, Module, Condition, ExtraData) ->
    _ = is_atom(Id) orelse error(badarg, [Id, Module, Condition, ExtraData]),
    _ = is_callback_module(Module) orelse error(badarg, [Id, Module, Condition, ExtraData]),
    _ = is_condition(Condition) orelse error(badarg, [Id, Module, Condition, ExtraData]),
    #?SINK{id = Id, module = Module, condition = Condition, extra_data = ExtraData}.

%% @doc Returns `true' if `X' is a sink, otherwise `false'
-spec is_sink(X :: (sink() | term())) -> boolean().
is_sink(X) -> is_record(X, ?SINK).

%% @doc Creates a new sink from `Map'
%%
%% Default Value: <br />
%% - id: the value of `module' <br />
%% - module: none (mandatory) <br />
%% - condition: `debug' <br />
%% - extra_data: `undefined' <br />
%%
%% ```
%% > logi_sink:to_map(logi_sink:from_map(#{module => logi_builtin_sink_null})).
%% #{condition => debug,
%%   extra_data => undefined,
%%   id => logi_builtin_sink_null,
%%   module => logi_builtin_sink_null}
%% '''
-spec from_map(Map :: map_form()) -> sink().
from_map(Map = #{module := Module}) ->
    new(maps:get(id, Map, Module), Module, maps:get(condition, Map, debug), maps:get(extra_data, Map, undefined));
from_map(Map) ->
    error(badarg, [Map]).

%% @doc Converts `Sink' into a map form
-spec to_map(Sink :: sink()) -> map_form().
to_map(#?SINK{id = Id, module = Module, condition = Condition, extra_data = ExtraData}) ->
    #{id => Id, module => Module, condition => Condition, extra_data => ExtraData}.

%% @doc Gets the ID of `Sink'
-spec get_id(Sink :: sink()) -> id().
get_id(#?SINK{id = Id}) -> Id.

%% @doc Gets the module of `Sink'
-spec get_module(Sink :: sink()) -> callback_module().
get_module(#?SINK{module = Module}) -> Module.

%% @doc Gets the condition of `Sink'
-spec get_condition(Sink :: sink()) -> condition().
get_condition(#?SINK{condition = Condition}) -> Condition.

%% @doc Gets the extra data of `Sink'
-spec get_extra_data(Sink :: sink()) -> extra_data().
get_extra_data(#?SINK{extra_data = ExtraData}) -> ExtraData.

%% @doc Gets the normalized condition of `Sink'
-spec get_normalized_condition(Sink :: sink()) -> normalized_condition().
get_normalized_condition(#?SINK{condition = Condition}) -> normalize_condition(Condition).

%% @doc Returns `true' if `X' is a valid `condition()' value, otherwise `false'
-spec is_condition(X :: (condition() | term())) -> boolean().
is_condition(X) when is_map(X) -> is_location_condition(X);
is_condition(X)                -> is_severity_condition(X).

%% @doc Returns `true' if `X' is a module which implements the `sink' behaviour, otherwise `false'
-spec is_callback_module(X :: (callback_module() | term())) -> boolean().
is_callback_module(X) ->
    _ = is_atom(X) orelse error(badarg, [X]),
    _ = code:is_loaded(X) =/= false orelse code:load_file(X),
    erlang:function_exported(X, write, 4).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_severity_condition(severity_condition() | term()) -> boolean().
is_severity_condition({Min, Max})                          -> logi:is_severity(Min) andalso logi:is_severity(Max);
is_severity_condition(Severities) when is_list(Severities) -> lists:all(fun logi:is_severity/1, Severities);
is_severity_condition(Min)                                 -> logi:is_severity(Min).

-spec is_location_condition(location_condition() | term()) -> boolean().
is_location_condition(Condition = #{severity := Severity}) ->
    is_location_condition(Severity) andalso is_location_condition(maps:without([severity], Condition));
is_location_condition(Condition = #{application := Application}) ->
    Valid =
        case is_list(Application) of
            false -> is_atom(Application);
            true  -> lists:all(fun erlang:is_atom/1, Application)
        end,
    Valid andalso is_location_condition(maps:without([application], Condition));
is_location_condition(Condition = #{module := Module}) ->
    Valid =
        case is_list(Module) of
            false -> is_module(Module);
            true  -> lists:all(fun is_module/1, Module)
        end,
    Valid andalso is_location_condition(maps:without([module], Condition));
is_location_condition(_) ->
    true.

-spec is_module(module() | term()) -> boolean().
is_module(Module) when is_atom(Module) -> guess_application(Module) =/= undefined;
is_module(_)                           -> false.

-spec guess_application(module()) -> atom() | undefined.
guess_application(Module) ->
    case application:get_application(Module) of
        {ok, Application} -> Application;
        undefined         -> undefined
    end.

-spec normalize_condition(condition()) -> normalized_condition().
normalize_condition(C) when is_map(C) -> normalize_location_condition(C);
normalize_condition(C)                -> normalize_severity_condition(C).

-spec normalize_location_condition(location_condition()) -> normalized_condition().
normalize_location_condition(C) ->
    #{severity := Severity, application := Application, module := Module} =
        maps:merge(#{severity => debug, application => [], module => []}, C),
    Applications = case is_list(Application) of true -> Application; false -> [Application] end,
    Modules = case is_list(Module) of true -> Module; false -> [Module] end,
    lists:usort(
      lists:append(
        [begin
             [{S, A} || A <- Applications] ++
             [{S, guess_application(M), M} || M <- Modules]
         end || S <- normalize_severity_condition(Severity)])).

-spec normalize_severity_condition(severity_condition()) -> normalized_condition().
normalize_severity_condition(C) ->
    L = fun (Severity) -> logi:severity_level(Severity) end,
    case C of
        {Min, Max}        -> lists:sublist(logi:severities(), L(Max), max(0, 1 + L(Min) - L(Max)));
        _ when is_atom(C) -> lists:sublist(logi:severities(), 1, L(C));
        _                 -> lists:usort(C)
    end.
