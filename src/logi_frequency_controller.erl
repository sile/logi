%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_frequency_controller).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         make/0,
         is_output_allowed/3
        ]).

-export_type([
              controller/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(logi_frequency_controller,
        {
          location_to_policy :: gb_trees:tree(location_id(), policy_state())
        }).

-record(once_in_times, {count :: pos_integer()}).
-record(interval,      {last :: erlang:timestamp(), omitted_count :: non_neg_integer()}).

-opaque controller() :: #logi_frequency_controller{}.

-type location_id() :: {module(), logi_location:line()}.

-type policy_state() :: once
                      | #once_in_times{}
                      | #interval{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc 出力頻度制御器を作成する
-spec make() -> controller().
make() ->
    #logi_frequency_controller{
       location_to_policy = gb_trees:empty()
      }.

%% @doc 出力が許可されているかどうかを判定する
-spec is_output_allowed(logi:frequency_policy_spec(), logi:location(), controller()) -> {{true, non_neg_integer()} | false, controller()}.
is_output_allowed(always, _Location, Controller) ->
    {{true, 0}, Controller};
is_output_allowed(PolicySpec, Location, Controller) ->
    #logi_frequency_controller{location_to_policy = LocationToPolicy} = Controller,
    LocationId = get_location_id(Location),
    {Result, PolicyState} = check_policy(PolicySpec, gb_trees:lookup(LocationId, LocationToPolicy)),
    LocationToPolicy2 = gb_trees:enter(LocationId, PolicyState, LocationToPolicy),
    {Result, Controller#logi_frequency_controller{location_to_policy = LocationToPolicy2}}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec get_location_id(logi_location:location()) -> location_id().
get_location_id(Location) ->
    {logi_location:get_module(Location), logi_location:get_line(Location)}.

-spec check_policy(logi:frequency_policy_spec(), MaybePolicyState) -> {boolean(), policy_state()} when
      MaybePolicyState :: {value, policy_state()} | none.
check_policy(once, MaybeState) ->
    case MaybeState =:= {value, once} of
        false -> {{true, 0}, once};
        true  -> {false, once}
    end;
check_policy({once_in_times, Times}, MaybeState) ->
    Count = case MaybeState of
                {value, #once_in_times{count = N}} -> N;
                _                                  -> 0
            end,
    case Count rem Times of
        0 -> {{true, max(0, Count - 1)}, #once_in_times{count = 1}};
        _ -> {false,                     #once_in_times{count = Count + 1}}
    end;
check_policy({interval, Internal}, MaybeState) ->
    Now = os:timestamp(),
    {Last, Count} =
        case MaybeState of
            {value, #interval{last = L, omitted_count = C}} -> {L, C};
            _                                               -> {{0, 0, 0}, 0}
        end,
    Elapsed = timer:now_diff(Now, Last) div 1000,
    case Elapsed >= Internal of
        true  -> {{true, Count}, #interval{last = Now,  omitted_count = 0}};
        false -> {false,         #interval{last = Last, omitted_count = Count + 1}}
    end;
check_policy(PolicySpec, MaybeState) ->
    error(badarg, [PolicySpec, MaybeState]).
