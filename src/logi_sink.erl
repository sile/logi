%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sink object
-module(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/3, new/4]).
-export([is_sink/1]).
-export([get_id/1, get_module/1, get_condition/1, get_extra_data/1]).
-export([get_expanded_condition/1]).
-export([to_map/1, from_map/1]).
-export([is_valid_condition/1]).

-export_type([sink/0]).
-export_type([id/0]).
-export_type([callback_module/0]).
-export_type([condition/0, severity_condition/0, location_condition/0, expanded_condition/0]).
-export_type([extra_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_context:context(), io:format(), [term()], extra_data()) -> any().

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

-opaque sink()      :: #?SINK{}.
-type id()              :: atom().
-type callback_module() :: module().
-type extra_data()      :: term().
-type condition()       :: severity_condition() | location_condition().

-type severity_condition() :: logi:log_level()
                            | {logi:log_level(), logi:log_level()}
                            | [logi:log_level()].
-type location_condition() ::
        #{
           severity    => severity_condition(),
           application => atom() | [atom()],
           module      => module() | [module()]
         }.

-type expanded_condition() :: [logi:log_level() |
                               {logi:log_level(), atom()} |
                               {logi:log_level(), atom(), module()}].

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Id, Module, debug)
-spec new(id(), callback_module()) -> sink().
new(Id, Module) -> new(Id, Module, debug).

%% @equiv new(Id, Module, Condition, undefined)
-spec new(id(), callback_module(), condition()) -> sink().
new(Id, Module, Condition) -> new(Id, Module, Condition, undefined).

%% @doc Makes a new sink
-spec new(id(), callback_module(), condition(), extra_data()) -> sink().
new(Id, Module, Condition, ExtraData) ->
    case is_atom(Id) andalso is_atom(Module) andalso is_valid_condition(Condition) of
        false -> error(badarg, [Id, Module, Condition, ExtraData]);
        true  -> #?SINK{id = Id, module = Module, condition = Condition, extra_data = ExtraData}
    end.

%% @doc TODO
-spec is_sink(sink() | term()) -> boolean().
is_sink(X) -> is_record(X, ?SINK).

%% @doc Makes a new sink from a map
%%
%% Default Value: <br />
%% - id: none (mandatory) <br />
%% - module: none (mandatory) <br />
%% - condition: `debug' <br />
%% - extra_data: `undefined' <br />
-spec from_map(Map) -> sink() when
      Map :: #{id => id(), module => callback_module(), condition => condition(), extra_data => extra_data()}.
from_map(Map) ->
    new(maps:get(id, Map), maps:get(module, Map), maps:get(condition, Map, debug), maps:get(extra_data, Map, undefined)).

%% @doc Converts `Sink' into a map
-spec to_map(Sink :: sink()) -> Map when
      Map :: #{id => id(), module => callback_module(), condition => condition(), extra_data => extra_data()}.
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

%% @doc Gets the expanded condition of `Sink'
-spec get_expanded_condition(Sink :: sink()) -> expanded_condition().
get_expanded_condition(#?SINK{condition = Condition}) -> expand_condition(Condition).

%% @doc Returns `true' if `X' is a valid `condition()' value, and `false' otherwise
-spec is_valid_condition(condition() | term()) -> boolean().
is_valid_condition(X) when is_map(X) -> is_valid_location_condition(X);
is_valid_condition(X)                -> is_valid_severity_condition(X).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_valid_severity_condition(severity_condition() | term()) -> boolean().
is_valid_severity_condition({MinSeverity, MaxSeverity}) ->
    lists:member(MinSeverity, logi:log_levels()) andalso lists:member(MaxSeverity, logi:log_levels());
is_valid_severity_condition(Severities) when is_list(Severities) ->
    lists:all(fun (S) -> lists:member(S, logi:log_levels()) end, Severities);
is_valid_severity_condition(MinSeverity) ->
    lists:member(MinSeverity, logi:log_levels()).

-spec is_valid_location_condition(location_condition() | term()) -> boolean().
is_valid_location_condition(Condition = #{severity := Severity}) ->
    is_valid_location_condition(Severity) andalso is_valid_location_condition(maps:without([severity], Condition));
is_valid_location_condition(Condition = #{application := Application}) ->
    Valid =
        case is_list(Application) of
            false -> is_atom(Application);
            true  -> lists:all(fun erlang:is_atom/1, Application)
        end,
    Valid andalso is_valid_location_condition(maps:without([application], Condition));
is_valid_location_condition(Condition = #{module := Module}) ->
    Valid =
        case is_list(Module) of
            false -> is_valid_module(Module);
            true  -> lists:all(fun is_valid_module/1, Module)
        end,
    Valid andalso is_valid_location_condition(maps:without([module], Condition));
is_valid_location_condition(_) ->
    true.

-spec is_valid_module(module() | term()) -> boolean().
is_valid_module(Module) when is_atom(Module) ->
    guess_application(Module) =/= undefined;
is_valid_module(_) ->
    false.

-spec guess_application(module()) -> atom() | undefined.
guess_application(Module) ->
    case application:get_application(Module) of
        {ok, Application} -> Application;
        undefined         -> undefined
    end.

-spec expand_condition(condition()) -> expanded_condition().
expand_condition(C) when is_map(C) -> expand_location_condition(C);
expand_condition(C)                -> expand_severity_condition(C).

-spec expand_location_condition(location_condition()) -> expanded_condition().
expand_location_condition(C) ->
    #{severity := Severity, application := Application, module := Module} =
        maps:merge(#{severity => debug, application => [], module => []}, C),
    Applications = case is_list(Application) of true -> Application; false -> [Application] end,
    Modules = case is_list(Module) of true -> Module; false -> [Module] end,
    lists:usort(
      lists:append(
        [begin
             [{S, A} || A <- Applications] ++
             [{S, guess_application(M), M} || M <- Modules]
         end || S <- expand_severity_condition(Severity)])).

-spec expand_severity_condition(severity_condition()) -> expanded_condition().
expand_severity_condition(C) when is_atom(C) ->
    drop_until(C, logi:log_levels());
expand_severity_condition({Min, Max}) ->
    drop_until(Max, lists:reverse(drop_until(Min, logi:log_levels())));
expand_severity_condition(C) ->
    lists:usort(C).

-spec drop_until(term(), list()) -> list().
drop_until(X, List) ->
    lists:dropwhile(fun (Y) -> X =/= Y end, List).
