%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Functions for sink processes
%%
%% A sink process manages the lifetime of a sink and a sink writer instance({@link logi_sink_writer}).
%%
%% Sink process is spawned at time a sink is installed in a channel ({@link logi_channel:install_sink/2}).
%%
%% After spawned, the process should call {@link send_writer_to_parent/1} to
%% notify available writer instance to the parent.
%%
%% If the root sink process exits, the associated sink is uninstalled from the channel.
%%
%% @end
-module(logi_sink_proc).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_child/1]).
-export([stop_child/1]).
-export([send_writer_to_parent/1]).
-export([recv_writer_from_child/2]).

-export_type([sink_sup/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_root_child/1]).
-export([register_child_sink/2]).
-export([register_grandchildren_sup/2]).
-export([whereis_child_sink/1]).
-export([whereis_grandchildren_sup/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type sink_sup() :: pid().
%% The supervisor of a sink process

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new child sink process
%%
%% NOTICE: This function can only be invoked in a sink process.
-spec start_child(logi_sink:sink()) -> {ok, sink_sup()} | {error, Reason::term()}.
start_child(Sink) ->
    ParentSup = get_parent_sup(),
    _ = is_pid(whereis_child_sink(ParentSup)) orelse register_child_sink(ParentSup, self()),
    logi_sink_sup:start_grandchild(ParentSup, Sink, false).

%% @doc Stops the sink process
%%
%% NOTICE: This function can only be invoked in a sink process.
-spec stop_child(sink_sup()) -> ok.
stop_child(SinkSup) ->
    ParentSup = get_parent_sup(),
    logi_sink_sup:stop_grandchild(ParentSup, SinkSup).

%% @doc Sends `Writer' to the parent sink process
%%
%% NOTICE: This function can only be invoked in a sink process.
-spec send_writer_to_parent(logi_sink_writer:writer() | undefined) -> ok.
send_writer_to_parent(Writer) ->
    _ = Writer =:= undefined orelse logi_sink_writer:is_writer(Writer) orelse error(badarg, [Writer]),
    Parent = get_parent_sink(),
    SinkSup = get_parent_sup(),
    _ = Parent ! {sink_writer, SinkSup, Writer},
    ok.

%% @doc Receives a sink writer instance from the child sink process `SinkSup'
-spec recv_writer_from_child(sink_sup(), timeout()) -> logi_sink_writer:writer() | undefined.
recv_writer_from_child(SinkSup, Timeout) ->
    receive
        {sink_writer, SinkSup, Writer} -> Writer
    after Timeout -> undefined
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec start_root_child(logi_sink:sink()) -> {ok, sink_sup()} | {error, Reason::term()}.
start_root_child(Sink) ->
    ParentSup = get_parent_sup(),
    _ = is_pid(whereis_child_sink(ParentSup)) orelse register_child_sink(ParentSup, self()),
    logi_sink_sup:start_grandchild(ParentSup, Sink, true).

%% @private
-spec register_child_sink(pid(), pid()) -> ok.
register_child_sink(ParentSup, ChildSink) ->
    Name = {ParentSup, child_sink},
    ok = logi_name_server:unregister_name(Name),
    yes = logi_name_server:register_name(Name, ChildSink),
    ok.

%% @private
-spec register_grandchildren_sup(pid(), pid()) -> ok.
register_grandchildren_sup(ParentSup, GrandChildrenSup) ->
    Name = {ParentSup, grandchildren_sup},
    ok = logi_name_server:unregister_name(Name),
    yes = logi_name_server:register_name(Name, GrandChildrenSup),
    ok.

%% @private
-spec whereis_child_sink(pid()) -> pid() | undefined.
whereis_child_sink(Sup) ->
    logi_name_server:whereis_name({Sup, child_sink}).

%% @private
-spec whereis_grandchildren_sup(pid()) -> pid() | undefined.
whereis_grandchildren_sup(Sup) ->
    logi_name_server:whereis_name({Sup, grandchildren_sup}).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec get_parent_sink() -> pid().
get_parent_sink() ->
    GrandParentSup = get_nth_parent_sup(2),
    case whereis_child_sink(GrandParentSup) of
        Pid when is_pid(Pid) -> Pid
    end.

-spec get_parent_sup() -> pid().
get_parent_sup() ->
    get_nth_parent_sup(1).

-spec get_nth_parent_sup(pos_integer()) -> pid().
get_nth_parent_sup(N) when is_integer(N), N > 0 ->
    (fun Loop ([Parent0 | Ancestors], I) ->
             Parent1 =
                 case is_atom(Parent0) of
                     true  -> whereis(Parent0);
                     false -> Parent0
                 end,
             case whereis_grandchildren_sup(Parent1) of
                 undefined    -> Loop(Ancestors, I);
                 _ when I > 1 -> Loop(Ancestors, I - 1);
                 _            -> Parent1
             end
     end)(get('$ancestors'), N).
