%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc client side frequency controller
%% @private
-module(logi_frequency).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/0]).
-export([flush_expired_entries/3]).
-export([is_output_allowed/3]).
-export([output_overflow_message/2]).

-export_type([controller/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-record(logi_frequency,
        {
          id_to_state = gb_trees:empty() :: gb_trees:tree(location_id(), state()),
          expires = logi_heap:new()      :: logi_heap:heap(expire_entry())
        }).

-opaque controller() :: #logi_frequency{}.

-type location_id() :: {module(), logi_location:line()}
                     | {module(), logi_location:line(), term()}.

-type expire_entry() :: {seconds(), location_id()}.
-type seconds() :: non_neg_integer(). % TODO: logi:seconds/0

-record(normal,
        {
          alive_count = 1 :: non_neg_integer()
        }).

-record(overflow,
        {
          alive_count       :: non_neg_integer(),
          dropped_count = 1 :: pos_integer(),
          msg_info          :: logi_msg_info:info(),
          on_expire         :: logi:on_expire_fun() % TODO: expire時に適用する関数と、それへの引数を返すインタフェースにする
        }).

-type state() :: #normal{} | #overflow{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Makes a frequency controller
-spec make() -> controller().
make() ->
    #logi_frequency{}.

%% TODO: doc
%-spec flush_expired_entries(non_neg_integer() | infinity, erlang:timestamps(), controller()) -> controller().
flush_expired_entries(_Count, _Timestamp, undefined) -> {undefined, []};
flush_expired_entries(0, _Timestamp, Controller)     -> {Controller, []};
flush_expired_entries(Count, Timestamp, Controller)  ->
    #logi_frequency{id_to_state = IdToState0, expires = Expires0} = Controller,
    Now =
        case Timestamp of
            infinity -> infinity;
            _        -> to_seconds(Timestamp)
        end,
    {IdToState1, Expires1, Entries} = flush_expired_entries(Count, Now, IdToState0, Expires0, []),
    {Controller#logi_frequency{id_to_state = IdToState1, expires = Expires1}, lists:reverse(Entries)}.

flush_expired_entries(0,    _Now, IdToState0, Expires0, Acc) ->
    {IdToState0, Expires0, Acc};
flush_expired_entries(FlushCount, Now, IdToState0, Expires0, Acc) ->
    case logi_heap:peek(Expires0) of
        empty                                      -> {IdToState0, Expires0, Acc};
        {{ExpiryTime, _}, _} when ExpiryTime > Now -> {IdToState0, Expires0, Acc};
        {{_, LocationId}, _}                       ->
            State0 = gb_trees:get(LocationId, IdToState0),
            {_, Expires1} = logi_heap:out(Expires0),
            {IdToState1, Acc1} =
                case State0 of
                    #normal{alive_count = 0}       -> {gb_trees:delete(LocationId, IdToState0), Acc};
                    #normal{alive_count = Count}   -> {gb_trees:update(LocationId, #normal{alive_count = Count - 1}, IdToState0), Acc};
                    #overflow{alive_count = Count} ->
                        %%ok = output_overflow_message(LocationId, State0), % TODO: 出力部分は外側に出す
                        X = {LocationId, State0},
                        {gb_trees:update(LocationId, #normal{alive_count = Count - 1}, IdToState0), [X | Acc]}
                end,
            flush_expired_entries(FlushCount - 1, Now, IdToState1, Expires1, Acc1)
    end.

output_overflow_message({LocationId, #overflow{on_expire = OnExpire, msg_info = MsgInfo, dropped_count = Count}}, Context) ->
    Id = case LocationId of
             {_, _}    -> undefined;
             {_, _, _} -> element(3, LocationId)
         end,
    OnExpire(Context, Id, Count, MsgInfo).

%% memo:
%% - period: ログ出力後に何秒間生存しているか
%% - intensity: ログ出力時の最大同時生存数

%% TODO: doc, spec, validate `Spec'
is_output_allowed(undefined, _MsgInfo, Controller) -> {true, Controller};
is_output_allowed(#{intensity := 0}, _, Controller)-> {false, Controller};
is_output_allowed(Spec, MsgInfo, undefined)        -> is_output_allowed(Spec, MsgInfo, make());
is_output_allowed(Spec, MsgInfo, Controller)       ->
    #logi_frequency{id_to_state = IdToState0, expires = Expires0} = Controller,
    LocationId = get_location_id(logi_msg_info:get_location(MsgInfo), Spec),
    Intensity = maps:get(intensity, Spec, 1),
    Period = maps:get(period, Spec, 60),
    case gb_trees:lookup(LocationId, IdToState0) of
        {value, State = #overflow{alive_count = Alives, dropped_count = Count}} when Alives >= Intensity ->
            IdToState1 = gb_trees:update(LocationId, State#overflow{dropped_count = Count + 1}, IdToState0),
            {false, Controller#logi_frequency{id_to_state = IdToState1}};
        {value, State = #overflow{alive_count = Alives}} ->
            IdToState1 = gb_trees:update(LocationId, State#overflow{alive_count = Alives + 1}, IdToState0),
            ExpiryTime = to_seconds(logi_msg_info:get_timestamp(MsgInfo)) + Period,
            Expires1 = logi_heap:in({ExpiryTime, LocationId}, Expires0),
            {true, Controller#logi_frequency{id_to_state = IdToState1, expires = Expires1}};
        Result ->
            State1 = #normal{alive_count = Count} =
                case Result of
                    none            -> #normal{};
                    {value, State0} -> #normal{alive_count = State0#normal.alive_count + 1}
                end,
            case maps:merge(#{on_expire => fun logi:on_expire/4}, Spec) of
                #{on_expire := OnExpire} when Count > Intensity ->
                    State2 = #overflow{alive_count = Count - 1, msg_info = MsgInfo, on_expire = OnExpire},
                    IdToState1 = gb_trees:update(LocationId, State2, IdToState0),
                    {false, Controller#logi_frequency{id_to_state = IdToState1}};
                _ ->
                    ExpiryTime = to_seconds(logi_msg_info:get_timestamp(MsgInfo)) + Period,
                    IdToState1 = gb_trees:enter(LocationId, State1, IdToState0),
                    Expires1 = logi_heap:in({ExpiryTime, LocationId}, Expires0),
                    {true, Controller#logi_frequency{id_to_state = IdToState1, expires = Expires1}}
            end
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec get_location_id(logi_location:location(), logi:frequency_policy()) -> location_id().
get_location_id(Location, #{id := Id}) ->
    {logi_location:get_module(Location), logi_location:get_line(Location), Id};
get_location_id(Location, _) ->
    {logi_location:get_module(Location), logi_location:get_line(Location)}.

-spec to_seconds(erlang:timestamp()) -> seconds().
to_seconds({MegaSeconds, Seconds, _}) ->
    MegaSeconds * 1000000 + Seconds.
