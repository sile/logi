%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc log output condition
%% @private
-module(logi_condition).


%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/1]).

-export_type([spec/0, severity_spec/0, location_spec/0]).
-export_type([condition/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(COND, ?MODULE).

-record(?COND,
        {
          spec :: spec()
        }).

-opaque condition() :: #?COND{}.

-type spec() :: severity_spec()
              | location_spec().

-type severity_spec() :: logi:log_level()
                       | {logi:log_level(), logi:log_level()}
                       | [logi:log_level()].
%% TODO: doc

-type location_spec() ::
        #{
           severity    => severity_spec(),
           application => atom() | [atom()],
           module      => module() | [module()]
         }.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Makes a condition object from `Spec'
-spec make(spec()) -> condition().
make(Spec) ->
    case is_valid_spec(Spec) of
        false -> error(badarg, [Spec]);
        true  -> #?COND{spec = Spec}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_valid_spec(spec() | term()) -> boolean().
is_valid_spec(Spec) when is_map(Spec) -> is_valid_location_spec(Spec);
is_valid_spec(Spec)                   -> is_valid_severity_spec(Spec).

-spec is_valid_severity_spec(severity_spec() | term()) -> boolean().
is_valid_severity_spec({MinSeverity, MaxSeverity}) ->
    lists:member(MinSeverity, logi:log_levels()) andalso lists:member(MaxSeverity, logi:log_levels());
is_valid_severity_spec(Severities) when is_list(Severities) ->
    lists:all(fun (S) -> lists:member(S, logi:log_levels()) end, Severities);
is_valid_severity_spec(MinSeverity) ->
    lists:member(MinSeverity, logi:log_levels()).

-spec is_valid_location_spec(location_spec() | term()) -> boolean().
is_valid_location_spec(Spec = #{severity := Severity}) ->
    is_valid_location_spec(Severity) andalso is_valid_location_spec(maps:without([severity], Spec));
is_valid_location_spec(Spec = #{application := Application}) ->
    Valid =
        case is_list(Application) of
            false -> is_atom(Application);
            true  -> lists:all(fun erlang:is_atom/1, Application)
        end,
    Valid andalso is_valid_location_spec(maps:without([application], Spec));
is_valid_location_spec(Spec = #{module := Module}) ->
    Valid =
        case is_list(Module) of
            false -> is_valid_module(Module);
            true  -> lists:all(fun is_valid_module/1, Module)
        end,
    Valid andalso is_valid_location_spec(maps:without([module], Spec));
is_valid_location_spec(_) ->
    true.

-spec is_valid_module(module() | term()) -> boolean().
is_valid_module(Module) when is_atom(Module) ->
    case application:get_application(Module) of
        {ok, _} -> true;
        _       -> false
    end;
is_valid_module(_) ->
    false.
