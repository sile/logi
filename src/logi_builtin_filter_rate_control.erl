%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in filter which controls output rate of log messages
-module(logi_builtin_filter_rate_control).

-behaviour(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0]).

-export_type([option/0, options/0]).
-export_type([rate_spec/0, rate_spec_id/0]).
-export_type([non_neg_seconds/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([filter/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          id_to_status = #{}        :: #{location_id() => status()},
          expires = logi_heap:new() :: logi_heap:heap(expire_entry())
        }).

-record(normal,
        {
          write_count = 1 :: non_neg_integer()
        }).

-record(overflow,
        {
          write_count    :: non_neg_integer(),
          drop_count = 1 :: non_neg_integer(),
          context        :: logi_context:context()
        }).

-type state() :: #?STATE{}.

-type options() :: [option()].
-type option() :: {max_rate, rate_spec()}.

-type rate_spec() ::
        #{
           id        => rate_spec_id(),
           intensity => non_neg_integer(),
           period    => non_neg_seconds()
         }.
%% TODO: doc

-type rate_spec_id() :: term(). % XXX: name

-type non_neg_seconds() :: non_neg_integer().

-type location_id() :: {module(), logi_location:line()}
                     | {module(), logi_location:line(), rate_spec_id()}.

-type expire_entry() :: {non_neg_seconds(), location_id()}.

-type status() :: #normal{} | #overflow{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a filter
-spec new() -> logi_filter:filter().
new() -> logi_filter:new(?MODULE, #?STATE{}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback API
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec filter(logi_context:context(), options(), state()) -> {boolean(), state()}.
filter(Context, Options, State0) ->
    State1 = flush_expired_entries(logi_context:get_timestamp(Context), State0),
    case proplists:get_value(max_rate, Options) of
        undefined         -> {true, State1};
        #{intensity := 0} -> {false, State1};
        Spec              ->
            _ = is_map(Spec) orelse error(badarg, [Context, Options, State1]),
            LocationId = get_location_id(logi_context:get_location(Context), Spec),
            Intensity = maps:get(intensity, Spec, 1),
            Period = maps:get(period, Spec, 60),
            _ = (is_integer(Intensity) andalso Intensity >= 0) orelse error(badarg, [Context, Options, State1]),
            _ = (is_integer(Period) andalso Period >= 0) orelse error(badarg, [Context, Options, State1]),
            #?STATE{id_to_status = IdToStatus0, expires = Expires0} = State1,
            case maps:find(LocationId, IdToStatus0) of
                {ok, Status = #overflow{write_count = Writes, drop_count = Drops}} when Writes >= Intensity ->
                    IdToStatus1 = maps:put(LocationId, Status#overflow{drop_count = Drops + 1}, IdToStatus0),
                    {false, State1#?STATE{id_to_status = IdToStatus1}};
                {ok, Status = #overflow{write_count = Writes}} ->
                    %% TODO: NOTE (maybe intensity changed)
                    IdToStatus1 = maps:put(LocationId, Status#overflow{write_count = Writes + 1}, IdToStatus0),
                    ExpiryTime = to_seconds(logi_context:get_timestamp(Context)) + Period,
                    Expires1 = logi_heap:in({ExpiryTime, LocationId}, Expires0),
                    {true, State1#?STATE{id_to_status = IdToStatus1, expires = Expires1}};
                Result ->
                    Status1 = #normal{write_count = Writes} =
                        case Result of
                            error         -> #normal{};
                            {ok, Status0} -> #normal{write_count = Status0#normal.write_count + 1}
                        end,
                    case Writes > Intensity of
                        true ->
                            Status2 = #overflow{write_count = Writes - 1, context = Context},
                            IdToStatus1 = maps:put(LocationId, Status2, IdToStatus0),
                            {false, State1#?STATE{id_to_status = IdToStatus1}};
                        false ->
                            ExpiryTime = to_seconds(logi_context:get_timestamp(Context)) + Period,
                            IdToStatus1 = maps:put(LocationId, Status1, IdToStatus0),
                            Expires1 = logi_heap:in({ExpiryTime, LocationId}, Expires0),
                            {true, State1#?STATE{id_to_status = IdToStatus1, expires = Expires1}}
                    end
            end
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec get_location_id(logi_location:location(), rate_spec()) -> location_id().
get_location_id(Location, #{id := Id}) -> {logi_location:get_module(Location), logi_location:get_line(Location), Id};
get_location_id(Location, _)           -> {logi_location:get_module(Location), logi_location:get_line(Location)}.

-spec to_seconds(erlang:timestamp()) -> non_neg_seconds().
to_seconds({MegaSeconds, Seconds, _}) -> MegaSeconds * 1000000 + Seconds.

-spec flush_expired_entries(erlang:timestamp(), #?STATE{}) -> #?STATE{}.
flush_expired_entries(Timestamp, State) ->
    #?STATE{id_to_status = IdToStatus0, expires = Expires0} = State,
    case logi_heap:is_empty(Expires0) of
        true  -> State;
        false ->
            Now = to_seconds(Timestamp),
            {IdToStatus1, Expires1} = flush_expired_entries(Now, IdToStatus0, Expires0),
            State#?STATE{id_to_status = IdToStatus1, expires = Expires1}
    end.

-spec flush_expired_entries(non_neg_seconds(), IdToStatus, Expires) -> {IdToStatus, Expires} when
      IdToStatus :: #{location_id() => status()},
      Expires :: logi_heap:heap(expire_entry()).
flush_expired_entries(Now, IdToStatus0, Expires0) ->
    case logi_heap:peek(Expires0) of
        empty                                 -> {IdToStatus0, Expires0};
        {ExpiryTime, _} when ExpiryTime > Now -> {IdToStatus0, Expires0};
        {_, LocationId}                       ->
            Status0 = maps:get(LocationId, IdToStatus0),
            Expires1 = logi_heap:out(Expires0),
            IdToStatus1 =
                case Status0 of
                    #normal{write_count = 0}   -> maps:remove(LocationId, IdToStatus0);
                    #normal{write_count = C}   -> maps:update(LocationId, #normal{write_count = C - 1}, IdToStatus0);
                    #overflow{write_count = C} ->
                        ok = output_overflow_message(LocationId, Now, Status0),
                        maps:update(LocationId, #normal{write_count = C - 1}, IdToStatus0)
                end,
            flush_expired_entries(Now, IdToStatus1, Expires1)
    end.

-spec output_overflow_message(location_id(), non_neg_seconds(), #overflow{}) -> ok.
output_overflow_message(LocationId, Now, #overflow{drop_count = Drops, context = Context}) ->
    Duration = Now - to_seconds(logi_context:get_timestamp(Context)),
    Severity = logi_context:get_severity(Context),
    _ = case LocationId of
        {_, _}     -> Id = undefined;
        {_, _, Id} -> Id
        end,
    _ = logi:Severity("Over a period of ~p seconds, ~p messages were dropped (rate_spec_id: ~p)", [Duration, Drops, Id],
                      [
                       {logger, [logi:new(logi_context:get_channel(Context))]},
                       {location, logi_context:get_location(Context)},
                       {headers, logi_context:get_headers(Context)},
                       {message, logi_context:get_metadata(Context)}
                      ]),
    ok.
