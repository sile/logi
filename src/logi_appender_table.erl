%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Appenders management table
%% @private
-module(logi_appender_table).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).
-export([register/3]).
-export([deregister/2]).
-export([which_appenders/1]).

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

%% @doc Registers an appender
-spec register(table(), logi_appender:appender(), logi_appender:appender() | undefined) -> ok.
register(Table, New, undefined) ->
    register(Table, New, logi_appender:make(dummy, dummy, []));
register(Table, New, Old) ->
    {Added, _, Deleted} = diff(logi_appender:get_expanded_condition(New), logi_appender:get_expanded_condition(Old)),
    ok = insert_appender(Table, New),
    ok = index_condition(Table, logi_appender:get_id(New), Added),
    ok = deindex_condition(Table, logi_appender:get_id(New), Deleted),
    ok.

%% @doc Deregisters an appender
-spec deregister(table(), logi_appender:appender()) -> ok.
deregister(Table, Appender) ->
    ok = deindex_condition(Table, logi_appender:get_id(Appender), logi_appender:get_expanded_condition(Appender)),
    ok = delete_appender(Table, Appender),
    ok.

%% @doc Returns a list of existing appenders
-spec which_appenders(table()) -> [logi_appender:id()].
which_appenders(Table) ->
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

-spec insert_appender(table(), logi_appender:appender()) -> ok.
insert_appender(Table, Appender) ->
    E = {logi_appender:get_id(Appender), {logi_appender:get_module(Appender), logi_appender:get_extra_data(Appender)}},
    _ = ets:insert(Table, E),
    ok.

-spec delete_appender(table(), logi_appender:appender()) -> ok.
delete_appender(Table, Appender) ->
    _ = ets:delete(Table, logi_appender:get_id(Appender)),
    ok.

-spec index_condition(table(), logi_appender:id(), logi_appender:expanded_condition()) -> ok.
index_condition(_Table, _AppenderId, []) ->
    ok;
index_condition(Table, AppenderId, [S | Condition]) when is_atom(S) ->
    ok = push_appender_id(Table, {S}, AppenderId),
    index_condition(Table, AppenderId, Condition);
index_condition(Table, AppenderId, [{S, A} | Condition]) ->
    ok = increment_descendant_count(Table, {S}),
    ok = push_appender_id(Table, {S, A}, AppenderId),
    index_condition(Table, AppenderId, Condition);
index_condition(Table, AppenderId, [{S, A, M} | Condition]) ->
    ok = increment_descendant_count(Table, {S}),
    ok = increment_descendant_count(Table, {S, A}),
    ok = push_appender_id(Table, {S, A, M}, AppenderId),
    index_condition(Table, AppenderId, Condition).

-spec deindex_condition(table(), logi_appender:id(), logi_appender:expanded_condition()) -> ok.
deindex_condition(_Table, _AppenderId, []) ->
    ok;
deindex_condition(Table, AppenderId, [S | Condition]) when is_atom(S) ->
    ok = pop_appender_id(Table, {S}, AppenderId),
    deindex_condition(Table, AppenderId, Condition);
deindex_condition(Table, AppenderId, [{S, A} | Condition]) ->
    ok = decrement_descendant_count(Table, {S}),
    ok = pop_appender_id(Table, {S, A}, AppenderId),
    deindex_condition(Table, AppenderId, Condition);
deindex_condition(Table, AppenderId, [{S, A, M} | Condition]) ->
    ok = decrement_descendant_count(Table, {S, A}),
    ok = decrement_descendant_count(Table, {S}),
    ok = pop_appender_id(Table, {S, A, M}, AppenderId),
    deindex_condition(Table, AppenderId, Condition).

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

-spec push_appender_id(table(), term(), logi_appender:id()) -> ok.
push_appender_id(Table, Key, AppenderId) ->
    [DescendantCount | AppenderIds] = fetch(Table, Key, [0]),
    _ = ets:insert(Table, {Key, [DescendantCount, AppenderId | AppenderIds]}),
    ok.

-spec pop_appender_id(table(), term(), logi_appender:id()) -> ok.
pop_appender_id(Table, Key, AppenderId) ->
    [DescendantCount | AppenderIds0] = fetch(Table, Key, [0]),
    AppenderIds1 = lists:delete(AppenderId, AppenderIds0),
    _ = case {DescendantCount, AppenderIds1} of
            {0, []} -> ets:delete(Table, Key);
            _       -> ets:insert(Table, {Key, [DescendantCount | AppenderIds1]})
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
