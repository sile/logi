-module(logi_sink_proc).

-export([start_child/1]).
-export([stop_child/1]).

-export([send_writer/1]). % to_parent
-export([recv_writer/2]). % from_child

-export_type([child_id/0]).

-type child_id() :: pid().

-spec get_parent_sink() -> pid().
get_parent_sink() ->
    [_, _, GrandParentSup | _] = get('$ancestors'),
    case logi_name_server:whereis_name({GrandParentSup, child_sink}) of
        undefined  -> error(unreachable); % assertion
        ParentSink -> ParentSink
    end.

-spec get_parent_sup() -> pid().
get_parent_sup() ->
    %% NOTE: sinkが独自の監視ツリーを持ち得ることを考慮すると、再帰的に辿らないとダメかもしれない
    [ParentSup | _] = get('$ancestors'),
    ParentSup.

-spec send_writer(logi_sink_writer:writer() | undefined) -> ok.
send_writer(Writer) ->
    Parent = get_parent_sink(),
    Id = get_parent_sup(),
    _ = Parent ! {sink_writer, Id, Writer},
    ok.

-spec recv_writer(child_id(), timeout()) -> logi_sink_writer:writer() | undefined.
recv_writer(ChildId, Timeout) ->
    receive
        {sink_writer, ChildId, Writer} -> Writer
    after Timeout -> undefined
    end.

-spec start_child(logi_sink:sink()) -> {ok, child_id()} | {error, Reason::term()}.
start_child(Sink) ->
    ParentSup = get_parent_sup(),
    ok = logi_name_server:register_name({ParentSup, child_sink}, self()),
    logi_sink_sup:start_grandchild(ParentSup, Sink).

-spec stop_child(child_id()) -> ok.
stop_child(ChildId) ->
    ParentSup = get_parent_sup(),
    logi_sink_sup:stop_grandchild(ParentSup, ChildId).
