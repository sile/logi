%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A logi_filter implementation to control log message output frequency
%% @end
%%
%% NOTE:
%% v0.0.12との互換性維持用モジュール.
%%
%% コード自体は https://github.com/sile/logi_stdlib/blob/master/src/logi_filter_frequency.erl からの移植.
-module(logi_builtin_filter_frequency).

-behaviour(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, new/1]).

-export_type([pos_milliseconds/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([filter/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(IS_POS_INT(X), (is_integer(X) andalso X > 0)).
-define(STATE, ?MODULE).

-record(?STATE,
        {
          id_to_status = #{}             :: #{location_id() => status()},
          expires = logi_builtin_util_heap:new() :: expires(),
          intensity                      :: intensity()
        }).

-record(intensity,
        {
          max_count = 5   :: pos_integer(),
          period  = 60000 :: pos_milliseconds()
        }).

-record(normal,
        {
          write_count = 0 :: non_neg_integer()
        }).

-record(overflow,
        {
          drop_count = 1 :: pos_integer(),
          context        :: logi_context:context()
        }).

-type status() :: #normal{} | #overflow{}.
-type expires() :: logi_builtin_util_heap:heap({ExpireTime::non_neg_milliseconds(), location_id()}).
-type location_id() :: {module(), logi_location:line(), logi:headers()}.
-type intensity() :: #intensity{}.
-type non_neg_milliseconds() :: non_neg_integer().

-type pos_milliseconds() :: pos_integer().
%% Positive milli-seconds

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new([])
-spec new() -> logi_filter:filter().
new() -> new([]).

%% @doc Creates a new filter instance
%%
%% === OPTIONS ===
%% `max_count':
%% - Maximum log message count allowed in the given period
%% - Default: `5'
%%
%% `period':
%% - Frequency control period
%% - Default: `60000'
-spec new(Options) -> logi_filter:filter() when
      Options :: [Option],
      Option  :: {max_count, pos_integer()}
               | {period, pos_milliseconds()}.
new(Options) ->
    _ = is_list(Options) orelse error(badarg, [Options]),
    MaxCount = proplists:get_value(max_count, Options, 5),
    Period = proplists:get_value(period, Options, 60000),
    _ = ?IS_POS_INT(MaxCount) orelse error(badarg, [Options]),
    _ = ?IS_POS_INT(Period) orelse error(badarg, [Options]),

    Intensity =
        #intensity{
           max_count = MaxCount,
           period    = Period
          },
    State = #?STATE{intensity = Intensity},
    logi_filter:new(?MODULE, State).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
filter(Context, State0) ->
    case logi_context:get_metadata(Context) of
        #{frequency_filter_enabled := true} ->
            State1 = flush_expired_entries(logi_context:get_timestamp(Context), State0),
            LocationId = get_location_id(Context),
            #?STATE{id_to_status = IdToStatus0, expires = Expires0, intensity = #intensity{max_count = MaxCount, period = Period}} = State1,
            Status0 = maps:get(LocationId, IdToStatus0, #normal{}),
            case Status0 of
                #overflow{drop_count = Drops} ->
                    IdToStatus1 = maps:put(LocationId, Status0#overflow{drop_count = Drops + 1}, IdToStatus0),
                    {false, State1#?STATE{id_to_status = IdToStatus1}};
                #normal{write_count = Writes} when Writes >= MaxCount ->
                    Status1 = #overflow{context = Context},
                    IdToStatus1 = maps:put(LocationId, Status1, IdToStatus0),
                    {false, State1#?STATE{id_to_status = IdToStatus1}};
                #normal{write_count = Writes} ->
                    Status1 = Status0#normal{write_count = Writes + 1},
                    IdToStatus1 = maps:put(LocationId, Status1, IdToStatus0),

                    ExpiryTime = to_milliseconds(logi_context:get_timestamp(Context)) + Period,
                    Expires1 = logi_builtin_util_heap:in({ExpiryTime, LocationId}, Expires0),

                    {true, State1#?STATE{id_to_status = IdToStatus1, expires = Expires1}}
            end;
        _ ->
            {true, State0}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec get_location_id(logi_context:context()) -> location_id().
get_location_id(Context) ->
    Location = logi_context:get_location(Context),
    {
      logi_location:get_module(Location),
      logi_location:get_line(Location),
      logi_context:get_headers(Context)
    }.

-spec to_milliseconds(erlang:timestamp()) -> non_neg_milliseconds().
to_milliseconds({Mega, Seconds, Micro}) ->
    (Mega * 1000 * 1000 * 1000) + (Seconds * 1000) + (Micro div 1000).

-spec flush_expired_entries(erlang:timestamp(), #?STATE{}) -> #?STATE{}.
flush_expired_entries(Now, State) ->
    #?STATE{id_to_status = IdToStatus0, expires = Expires0, intensity = Intensity} = State,
    case logi_builtin_util_heap:is_empty(Expires0) of
        true  -> State;
        false ->
            {IdToStatus1, Expires1} = flush_expired_entries(to_milliseconds(Now), IdToStatus0, Expires0, Intensity),
            State#?STATE{id_to_status = IdToStatus1, expires = Expires1}
    end.

-spec flush_expired_entries(non_neg_milliseconds(), IdToStatus, Expires, intensity()) -> {IdToStatus, Expires} when
      IdToStatus :: #{location_id() => status()},
      Expires    :: expires().
flush_expired_entries(Now, IdToStatus0, Expires0, Intensity) ->
    case logi_builtin_util_heap:peek(Expires0) of
        empty                                 -> {IdToStatus0, Expires0};
        {ExpiryTime, _} when ExpiryTime > Now -> {IdToStatus0, Expires0};
        {_, LocationId}                       ->
            Status = maps:get(LocationId, IdToStatus0),
            Expires1 = logi_builtin_util_heap:out(Expires0),
            IdToStatus1 =
                case Status of
                    #normal{write_count = 1} -> maps:remove(LocationId, IdToStatus0);
                    #normal{write_count = C} -> maps:update(LocationId, #normal{write_count = C - 1}, IdToStatus0);
                    #overflow{}              ->
                        _ = output_overflow_message(Now, Status),
                        MaxCount = Intensity#intensity.max_count,
                        maps:update(LocationId, #normal{write_count = MaxCount - 1} ,IdToStatus0)
                end,
            flush_expired_entries(Now, IdToStatus1, Expires1, Intensity)
    end.

-spec output_overflow_message(non_neg_milliseconds(), #overflow{}) -> any().
output_overflow_message(Now, #overflow{drop_count = Drops, context = Context}) ->
    Duration = (Now - to_milliseconds(logi_context:get_timestamp(Context))) / 1000,
    Severity = logi_context:get_severity(Context),
    logi:Severity("Over a period of ~p seconds, ~p messages were dropped", [Duration, Drops],
                  [
                   {logger,   logi:new([{channel, logi_context:get_channel(Context)}])},
                   {location, logi_context:get_location(Context)},
                   {headers,  logi_context:get_headers(Context)},
                   {metadata, logi_context:get_metadata(Context)}
                  ]).
