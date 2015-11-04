%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sinks
%%
%% A sink will consume the log messages sent to the channel which the sink have been installed.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > ok = logi_channel:create(sample_log).
%% > WriteFun = fun (_, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
%% > Sink = logi_sink:new(my_sink, logi_builtin_sink_fun, info, WriteFun).
%% > {ok, _} = logi_channel:install_sink(sample_log, Sink).
%% > logi:info("Hello World", [], [{logger, sample_log}]).
%% [my_sink] Hello World  % 'logi_builtin_sink_fun:write/4' was invoked
%% </pre>
%%
%% Conventionally, sink implementation modules provide `install' function to install the sink.
%% <pre lang="erlang">
%% > ok = logi_channel:create(sample_log).
%% > WriteFun = fun (_, _, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
%% > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun, [{channel, sample_log}]).
%% > logi:info("Hello World", [], [{logger, sample_log}]).
%% [my_sink] Hello World  % 'logi_builtin_sink_fun:write/4' was invoked
%% </pre>
%%
%% A channel can have multiple sinks.
%% <pre lang="erlang">
%% > ok = logi_channel:create(sample_log).
%% > WriteFun_0 = fun (_, _, Format, Data) -> io:format("[sink_0] " ++ Format ++ "\n", Data) end.
%% > WriteFun_1 = fun (_, _, Format, Data) -> io:format("[sink_1] " ++ Format ++ "\n", Data) end.
%% > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun_0, [{id, sink_0}, {channel, sample_log}]).
%% > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun_1, [{id, sink_1}, {channel, sample_log}]).
%% > logi:info("Hello World", [], [{logger, sample_log}]).
%% [sink_0] Hello World
%% [sink_1] Hello World
%% </pre>
-module(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2, unsafe_new/2]).
-export([is_sink/1]).
-export([get_module/1, get_extra_data/1]).
-export([normalize_condition/1]).
-export([is_condition/1]).
-export([is_callback_module/1]).

-export([default_layout/1]).
-export([write/5]).

-export_type([sink/0]).
-export_type([id/0]).
-export_type([callback_module/0]).
-export_type([condition/0, severity_condition/0, location_condition/0, normalized_condition/0]).
-export_type([extra_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_context:context(), logi_layout:layout(), io:format(), logi_layout:data(), extra_data()) -> any().
-callback default_layout(extra_data()) -> logi_layout:layout().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque sink() :: {callback_module(), extra_data()}.
%% A sink.

-type id() :: atom().
%% The identifier of a sink.
%% The sinks installed in the same channel must have different identifiers.

-type callback_module() :: module().
%% A module that implements the `logi_sink' behaviour.

-type extra_data() :: term().
%% The value of the fourth arguemnt of the `write/4' callback function.
%%
%% NOTE:
%% This value will be loaded from ETS every time the `write/4' is called.
%% Therefore, very huge data can cause a performance issue.

-type condition() :: severity_condition() | location_condition().
%% The condition to determine which messages to be consumed by a sink.

-type severity_condition() :: (Min :: logi:severity())
                            | {Min :: logi:severity(), Max :: logi:severity()}
                            | (Severities :: [logi:severity()]).
%% `Min':
%% - The messages with `Min' or higher severity will be consumed.
%%
%% `{Min, Max}':
%% - The messages with severity between `Min' and `Max' will be consumed.
%%
%% `Severities':
%% - The messages with severity included in `Severities' will be consumed.
%%
%% === EXAMPLE ===
%% <pre lang="erlang">
%% > logi_sink:new(Id, Module, info).          % level
%% > logi_sink:new(Id, Module, {info, alert}). % range
%% > logi_sink:new(Id, Module, [info, alert]). % list
%% </pre>

-type location_condition() ::
        #{
           severity    => severity_condition(),
           application => logi_location:application() | [logi_location:application()],
           module      => module() | [module()]
         }.
%% The messages which satisfy `severity' (default is `debug') and are sent from the specified location will be consumed.
%%
%% The location is specified by `application' and `module' (OR condition).
%%
%% NOTE: The modules which does not belong to any application are forbidden.
%%
%% === EXAMPLE ===
%% <pre lang="erlang">
%% > logi_sink:new(Id, Module, #{application => stdlib}).                          % application
%% > logi_sink:new(Id, Module, #{application => [stdlib, kernel]}).                % applications
%% > logi_sink:new(Id, Module, #{module => lists}).                                % module
%% > logi_sink:new(Id, Module, #{module => [lists, dict]}).                        % modules
%% > logi_sink:new(Id, Module, #{application => kernel, module => [lists, dict]}). % application and modules
%% > logi_sink:new(Id, Module, #{severity => [info, alert], module => lists}).     % severity and module
%% </pre>

-type normalized_condition() :: [logi:severity() |
                                 {logi:severity(), logi_location:application()} |
                                 {logi:severity(), logi_location:application(), module()}].
%% The normalized form of a `condition/0'.
%%
%% <pre lang="erlang">
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
%% </pre>

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Module, undefined)
-spec new(callback_module()) -> sink().
new(Module) -> new(Module, undefined).

%% @doc Creates a new sink instance
-spec new(callback_module(), extra_data()) -> sink().
new(Module, ExtraData) ->
    _ = is_callback_module(Module) orelse error(badarg, [Module, ExtraData]),
    unsafe_new(Module, ExtraData).

%% @doc Creates a new sink instance without validating the arguments
-spec unsafe_new(callback_module(), extra_data()) -> sink().
unsafe_new(Module, ExtraData) -> {Module, ExtraData}.

%% @doc Returns `true' if `X' is a sink, otherwise `false'
-spec is_sink(X :: (sink() | term())) -> boolean().
is_sink({Module, _}) -> is_callback_module(Module);
is_sink(_)           -> false.

%% @doc Gets the module of `Sink'
-spec get_module(Sink :: sink()) -> callback_module().
get_module({Module, _}) -> Module.

%% @doc Gets the extra data of `Sink'
-spec get_extra_data(Sink :: sink()) -> extra_data().
get_extra_data({_, ExtraData}) -> ExtraData.

%% @doc Returns a normalized form of `Condition'
-spec normalize_condition(Condition :: condition()) -> normalized_condition().
normalize_condition(C) when is_map(C) -> normalize_location_condition(C);
normalize_condition(C)                -> normalize_severity_condition(C).

%% @doc Returns `true' if `X' is a valid `condition()' value, otherwise `false'
-spec is_condition(X :: (condition() | term())) -> boolean().
is_condition(X) when is_map(X) -> is_location_condition(X);
is_condition(X)                -> is_severity_condition(X).

%% @doc Returns `true' if `X' is a module which implements the `sink' behaviour, otherwise `false'
-spec is_callback_module(X :: (callback_module() | term())) -> boolean().
is_callback_module(X) ->
    logi_utils:function_exported(X, default_layout, 1) andalso logi_utils:function_exported(X, write, 5).

%% @doc Returns the default layout of `Sink'
-spec default_layout(Sink :: sink()) -> logi_layout:layout().
default_layout({Module, ExtraData}) -> Module:default_layout(ExtraData).

%% @doc Writes a log message
-spec write(logi_context:context(), logi_layout:layout(), io:format(), logi_layout:data(), sink()) -> any().
write(Context, Layout, Format, Data, {Module, ExtraData}) ->
    Module:write(Context, Layout, Format, Data, ExtraData).

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
is_module(Module) when is_atom(Module) -> logi_location:guess_application(Module) =/= undefined;
is_module(_)                           -> false.

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
             [{S, logi_location:guess_application(M), M} || M <- Modules]
         end || S <- normalize_severity_condition(Severity)])).

-spec normalize_severity_condition(severity_condition()) -> normalized_condition().
normalize_severity_condition(C) ->
    L = fun (Severity) -> logi:severity_level(Severity) end,
    case C of
        {Min, Max}        -> lists:sublist(logi:severities(), L(Max), max(0, 1 + L(Min) - L(Max)));
        _ when is_atom(C) -> lists:sublist(logi:severities(), 1, L(C));
        _                 -> lists:usort(C)
    end.
