%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sinks management table
%% @private
-module(logi_sink_table).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).
-export([register/3]).
-export([deregister/2]).
-export([which_sinks/1]).

-export_type([table/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type table() :: ets:tab().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new table for the logger `LoggerId'
-spec new(logi:logger_id()) -> table().
new(LoggerId) ->
    ets:new(LoggerId, [set, protected, {read_concurrency, true}, named_table]).

%% @doc Registers an sink
-spec register(table(), logi_sink:sink(), logi_sink:sink() | undefined) -> ok.
register(Table, New, undefined) ->
    register(Table, New, logi_sink:new(dummy, dummy, []));
register(Table, New, Old) ->
    {Added, _, Deleted} = diff(logi_sink:get_expanded_condition(New), logi_sink:get_expanded_condition(Old)),
    ok = insert_sink(Table, New),
    ok = index_condition(Table, logi_sink:get_id(New), Added),
    ok = deindex_condition(Table, logi_sink:get_id(New), Deleted),
    ok.

%% @doc Deregisters an sink
-spec deregister(table(), logi_sink:sink()) -> ok.
deregister(Table, Sink) ->
    ok = deindex_condition(Table, logi_sink:get_id(Sink), logi_sink:get_expanded_condition(Sink)),
    ok = delete_sink(Table, Sink),
    ok.

%% @doc Returns a list of existing sinks
-spec which_sinks(table()) -> [logi_sink:id()].
which_sinks(Table) ->
    [Id || {Id, _} <- ets:tab2list(Table), is_atom(Id)].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec diff(A::list(), B::list()) -> {OnlyA::list(), Common::list(), OnlyB::list()}.
diff(A, B) ->
    As = ordsets:from_list(A),
    Bs = ordsets:from_list(B),
    {
      ordsets:to_list(ordsets:subtract(As, Bs)),
      ordsets:to_list(ordsets:intersection(As, Bs)),
      ordsets:to_list(ordsets:subtract(Bs, As))
    }.

-spec insert_sink(table(), logi_sink:sink()) -> ok.
insert_sink(Table, Sink) ->
    E = {logi_sink:get_id(Sink), {logi_sink:get_module(Sink), logi_sink:get_extra_data(Sink)}},
    _ = ets:insert(Table, E),
    ok.

-spec delete_sink(table(), logi_sink:sink()) -> ok.
delete_sink(Table, Sink) ->
    _ = ets:delete(Table, logi_sink:get_id(Sink)),
    ok.

-spec index_condition(table(), logi_sink:id(), logi_sink:expanded_condition()) -> ok.
index_condition(_Table, _SinkId, []) ->
    ok;
index_condition(Table, SinkId, [S | Condition]) when is_atom(S) ->
    ok = push_sink_id(Table, {S}, SinkId),
    index_condition(Table, SinkId, Condition);
index_condition(Table, SinkId, [{S, A} | Condition]) ->
    ok = increment_descendant_count(Table, {S}),
    ok = push_sink_id(Table, {S, A}, SinkId),
    index_condition(Table, SinkId, Condition);
index_condition(Table, SinkId, [{S, A, M} | Condition]) ->
    ok = increment_descendant_count(Table, {S}),
    ok = increment_descendant_count(Table, {S, A}),
    ok = push_sink_id(Table, {S, A, M}, SinkId),
    index_condition(Table, SinkId, Condition).

-spec deindex_condition(table(), logi_sink:id(), logi_sink:expanded_condition()) -> ok.
deindex_condition(_Table, _SinkId, []) ->
    ok;
deindex_condition(Table, SinkId, [S | Condition]) when is_atom(S) ->
    ok = pop_sink_id(Table, {S}, SinkId),
    deindex_condition(Table, SinkId, Condition);
deindex_condition(Table, SinkId, [{S, A} | Condition]) ->
    ok = decrement_descendant_count(Table, {S}),
    ok = pop_sink_id(Table, {S, A}, SinkId),
    deindex_condition(Table, SinkId, Condition);
deindex_condition(Table, SinkId, [{S, A, M} | Condition]) ->
    ok = decrement_descendant_count(Table, {S, A}),
    ok = decrement_descendant_count(Table, {S}),
    ok = pop_sink_id(Table, {S, A, M}, SinkId),
    deindex_condition(Table, SinkId, Condition).

-spec increment_descendant_count(table(), term()) -> ok.
increment_descendant_count(Table, Key) ->
    [Count | List] = fetch(Table, Key, [0]),
    _ = ets:insert(Table, {Key, [Count + 1 | List]}),
    ok.

-spec decrement_descendant_count(table(), term()) -> ok.
decrement_descendant_count(Table, Key) ->
    _ = case fetch(Table, Key) of
            [1]                           -> ets:delete(Table, Key);
            [Count | List] when Count > 1 -> ets:insert(Table, {Key, [Count - 1 | List]})
        end,
    ok.

-spec push_sink_id(table(), term(), logi_sink:id()) -> ok.
push_sink_id(Table, Key, SinkId) ->
    [DescendantCount | SinkIds] = fetch(Table, Key, [0]),
    _ = ets:insert(Table, {Key, [DescendantCount, SinkId | SinkIds]}),
    ok.

-spec pop_sink_id(table(), term(), logi_sink:id()) -> ok.
pop_sink_id(Table, Key, SinkId) ->
    [DescendantCount | SinkIds0] = fetch(Table, Key, [0]),
    SinkIds1 = lists:delete(SinkId, SinkIds0),
    _ = case {DescendantCount, SinkIds1} of
            {0, []} -> ets:delete(Table, Key);
            _       -> ets:insert(Table, {Key, [DescendantCount | SinkIds1]})
        end,
    ok.

-spec fetch(table(), term()) -> term().
fetch(Table, Key) ->
    case ets:lookup(Table, Key) of
        []       -> error({no_such_key, Key}, [Table, Key]);
        [{_, V}] -> V
    end.

-spec fetch(table(), term(), term()) -> term().
fetch(Table, Key, Default) ->
    case ets:lookup(Table, Key) of
        []       -> Default;
        [{_, V}] -> V
    end.
