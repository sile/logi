%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_sink_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([start_grandchild/2]).
-export([stop_grandchild/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a supervisor
-spec start_link(supervisor:sup_flags()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Flags) ->
    supervisor:start_link(?MODULE, [Flags]).

-spec start_grandchild(pid(), logi_sink:sink()) -> {ok, logi_sink_proc:child_id()} | {error, Reason::term()}.
start_grandchild(Sup, GrandChildSink) ->
    %% TODO: 用語整理
    GrandChildSup = logi_name_server:whereis_name({Sup, grandchildren_sup}),
    case logi_sink_set_sup:start_sink_sup(GrandChildSup, logi_sink:get_sup_flags(GrandChildSink)) of
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
    case logi_name_server:whereis_name({Sup, grandchildren_sup}) of
        undefined     -> ok;
        GrandChildSup -> logi_sink_set_sup:stop_sink_sup(GrandChildSup, GrandChildId)
    end.

-spec start_sink(pid(), supervisor:child_spec()) -> {ok, pid()} | {error, Reason::term()}.
start_sink(Sup, ChildSpec) ->
    case supervisor:start_child(Sup, ChildSpec) of
        {error, Reason} -> {error, Reason};
        {ok, undefined} -> {error, {ignored, ChildSpec}};
        {ok, Pid}       -> {ok, Pid}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Flags]) ->
    ChildrenSup = #{id => child_sink_set_sup, start => {logi_sink_set_sup, start_link, []}, type => supervisor},
    {ok, {Flags, [ChildrenSup]}}.
