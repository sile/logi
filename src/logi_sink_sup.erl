%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_sink_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/2]).
-export([start_grandchild/3]).
-export([stop_grandchild/2]).
-export([get_child/1]).
-export([find_grandchild/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a supervisor
-spec start_link(logi_sink:id(), supervisor:sup_flags()) -> {ok, pid()} | {error, Reason::term()}.
start_link(SinkId, Flags) ->
    supervisor:start_link({via, logi_name_server, {sink, self(), SinkId}}, ?MODULE, [Flags]).

-spec start_grandchild(pid(), logi_sink:sink(), boolean()) -> {ok, logi_sink_proc:child_id()} | {error, Reason::term()}.
start_grandchild(Sup, GrandChildSink, IsRoot) ->
    %% TODO: 用語整理
    GrandChildSup = logi_sink_proc:whereis_grandchildren_sup(Sup),
    Id = case IsRoot of
             false -> logi_sink:get_id(GrandChildSink);
             true  -> make_ref()
         end,
    case logi_sink_set_sup:start_sink_sup(GrandChildSup, Id, logi_sink:get_sup_flags(GrandChildSink)) of
        {error, Reason} -> {error, Reason};
        {ok, SinkSup}   ->
            try
                case start_sink(SinkSup, logi_sink:get_spec(GrandChildSink)) of
                    {error, Reson} ->
                        ok = logi_sink_set_sup:stop_sink_sup(GrandChildSup, SinkSup),
                        {error, Reson};
                    {ok, _} ->
                        {ok, SinkSup}
                end
            catch
                ExClass:ExReason ->
                    ok = logi_sink_set_sup:stop_sink_sup(GrandChildSup, SinkSup),
                    erlang:raise(ExClass, ExReason, erlang:get_stacktrace())
            end
    end.

-spec stop_grandchild(pid(), logi_sink_proc:child_id()) -> ok.
stop_grandchild(Sup, GrandChildId) ->
    case logi_sink_proc:whereis_grandchildren_sup(Sup) of
        undefined     -> ok;
        GrandChildSup -> logi_sink_set_sup:stop_sink_sup(GrandChildSup, GrandChildId)
    end.

-spec get_child(pid()) -> pid() | undefined.
get_child(Sup) ->
    case [Pid || {Id, Pid, _, _} <- supervisor:which_children(Sup), Id =/= child_sink_set_sup] of
        [P] when is_pid(P) -> P;
        _                  -> undefined
    end.

-spec find_grandchild(pid(), logi_sink:id()) -> {ok, pid()} | error.
find_grandchild(Sup, SinkId) ->
    case logi_name_server:whereis_name({sink, Sup, SinkId}) of
        undefined -> error;
        Pid       -> {ok, Pid}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Flags]) ->
    ChildrenSup = #{id => child_sink_set_sup, start => {logi_sink_set_sup, start_link, []}, type => supervisor},
    {ok, {maps:merge(Flags, #{strategy => one_for_all}), [ChildrenSup]}}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_sink(pid(), supervisor:child_spec()) -> {ok, pid()} | {error, Reason::term()}.
start_sink(Sup, ChildSpec) ->
    case supervisor:start_child(Sup, ChildSpec) of
        {error, Reason} -> {error, Reason};
        {ok, undefined} -> {error, {ignored, ChildSpec}};
        {ok, Pid}       -> {ok, Pid}
    end.
