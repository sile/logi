%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sinks
%%
%% A sink will consume the log messages sent to the channel which the sink have been installed.
%%
%% The main purpose of sinks is to write messages to some output devices (e.g. tty, file, socket).
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% > ok = logi_channel:create(sample_log).
%% > WriteFun = fun (_, _, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
%% > Sink = logi_builtin_sink_fun:new(WriteFun).
%% > {ok, _} = logi_channel:install_sink(info, Sink, [{id, my_sink}, {channel, sample_log}]).
%% > logi:info("Hello World", [], [{logger, sample_log}]).
%% [my_sink] Hello World  % 'logi_builtin_sink_fun:write/4' was invoked
%% </pre>
%%
%% Sinks have an associated layout:
%% <pre lang="erlang">
%% > WriteFun = fun (Context, Layout, Format, Data) -> io:format(logi_layout:format(Context, Format, Data, Layout)) end.
%% > Sink = logi_builtin_sink_fun:new(WriteFun).
%% > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[EXAMPLE] " ++ Format ++"\n", Data) end).
%% > {ok, _} = logi_channel:install_sink(info, Sink, [{layout, Layout}]). % Installs `Sink' to the default channel
%% > logi:info("hello world").
%% [EXAMPLE]hello world
%%
%% %% If 'layout' option is not specified, the result of `logi_sink:default_layout(Sink)' will be used instead.
%% > {ok, _} = logi_channel:install_sink(info, Sink, [{if_exists, supersede}]).
%% > logi:info("hello world").
%% 2015-11-09 22:18:33.934 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
%%
%% A channel can have multiple sinks:
%% <pre lang="erlang">
%% > ok = logi_channel:create(sample_log).
%% > WriteFun_0 = fun (_, _, Format, Data) -> io:format("[sink_0] " ++ Format ++ "\n", Data) end.
%% > WriteFun_1 = fun (_, _, Format, Data) -> io:format("[sink_1] " ++ Format ++ "\n", Data) end.
%% > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun_0), [{id, sink_0}, {channel, sample_log}]).
%% > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun_1), [{id, sink_1}, {channel, sample_log}]).
%% > logi:info("Hello World", [], [{logger, sample_log}]).
%% [sink_0] Hello World
%% [sink_1] Hello World
%% </pre>
-module(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/3]).
-export([is_sink/1]).
-export([get_module/1, get_layout/1, get_extra_data/1]).
-export([normalize_condition/1]).
-export([is_condition/1]).
-export([is_callback_module/1]).

-export([write/4]).

-export_type([sink/0]).
-export_type([id/0]).
-export_type([callback_module/0]).
-export_type([condition/0, severity_condition/0, location_condition/0, normalized_condition/0]).
-export_type([extra_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_context:context(), logi_layout:formatted_data(), extra_data()) -> any().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque sink() :: {callback_module(), logi_layout:layout(), extra_data()}.
%% A sink instance.

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
%% > [emergency,alert]     = logi_sink:normalize_condition(alert).               % level
%% > [warning,notice,info] = logi_sink:normalize_condition({info, warning}).      % range
%% > [alert,debug,info]    = logi_sink:normalize_condition([debug, info, alert]). % list
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
%% > logi_sink:is_condition(#{application => stdlib}).                          % application
%% > logi_sink:is_condition(#{application => [stdlib, kernel]}).                % applications
%% > logi_sink:is_condition(#{module => lists}).                                % module
%% > logi_sink:is_condition(#{module => [lists, dict]}).                        % modules
%% > logi_sink:is_condition(#{application => kernel, module => [lists, dict]}). % application and modules
%% > logi_sink:is_condition(#{severity => [info, alert], module => lists}).     % severity and module
%% </pre>

-type normalized_condition() :: [logi:severity() |
                                 {logi:severity(), logi_location:application()} |
                                 {logi:severity(), logi_location:application(), module()}].
%% The normalized form of a `condition/0'.
%%
%% <pre lang="erlang">
%% > Normalize = fun (C) -> lists:sort(logi_sink:normalize_condition(C)) end.
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
%% @equiv new(Module, Layout, undefined)
-spec new(callback_module(), logi_layout:layout()) -> sink().
new(Module, Layout) -> new(Module, Layout, undefined).

%% @doc Creates a new sink instance
-spec new(callback_module(), logi_layout:layout(), extra_data()) -> sink().
new(Module, Layout, ExtraData) ->
    _ = is_callback_module(Module) orelse error(badarg, [Module, Layout, ExtraData]),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Module, Layout, ExtraData]),
    {Module, Layout, ExtraData}.

%% @doc Returns `true' if `X' is a sink, otherwise `false'
-spec is_sink(X :: (sink() | term())) -> boolean().
is_sink({Module, _, _}) -> is_callback_module(Module);
is_sink(_)              -> false.

%% @doc Gets the module of `Sink'
-spec get_module(Sink :: sink()) -> callback_module().
get_module({Module, _, _}) -> Module.

%% @doc Gets the layout of `Sink'
-spec get_layout(Sink :: sink()) -> logi_layout:layout().
get_layout({_, Layout, _}) -> Layout.

%% @doc Gets the extra data of `Sink'
-spec get_extra_data(Sink :: sink()) -> extra_data().
get_extra_data({_, _, ExtraData}) -> ExtraData.

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
is_callback_module(X) -> logi_utils:function_exported(X, write, 3).

%% @doc Writes a log message
%%
%% If it fails to write, an exception will be raised.
-spec write(logi_context:context(), io:format(), logi_layout:data(), sink()) -> any().
write(Context, Format, Data, {Module, Layout, ExtraData}) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    Module:write(Context, FormattedData, ExtraData).

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
