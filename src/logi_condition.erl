%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sink Applicable Condition
%% @end
-module(logi_condition).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([is_condition/1]).
-export([normalize/1]).

-export_type([condition/0]).
-export_type([severity_condition/0]).
-export_type([location_condition/0]).
-export_type([normalized_condition/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
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
%% > [emergency,alert]     = logi_sink:normalize_condition(alert).                % level
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
%% @doc Returns `true' if `X' is a valid `condition()' value, otherwise `false'
-spec is_condition(X :: (condition() | term())) -> boolean().
is_condition(X) when is_map(X) -> is_location_condition(X);
is_condition(X)                -> is_severity_condition(X).

%% @doc Returns a normalized form of `Condition'
-spec normalize(Condition :: condition()) -> normalized_condition().
normalize(C) when is_map(C) -> normalize_location_condition(C);
normalize(C)                -> normalize_severity_condition(C).

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
