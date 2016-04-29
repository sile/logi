%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
%%
%% TODO: 動作確認用に logi_builtin_sink_composit 的なモジュールを作成する
-module(logi_sink_set_sup). % TODO: rename

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_sink_sup/3]).
-export([stop_sink_sup/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link(?MODULE, [self()]).

-spec start_sink_sup(pid(), logi_sink:id(), supervisor:sup_flags()) -> {ok, logi_sink_proc:child_id()} | {error, Reason::term()}.
start_sink_sup(Sup, SinkId, Flags) ->
    supervisor:start_child(Sup, [SinkId, Flags]).

-spec stop_sink_sup(pid(), logi_sink_proc:child_id()) -> ok.
stop_sink_sup(Sup, SinkSup) ->
    _ = supervisor:terminate_child(Sup, SinkSup),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([ParentSup]) ->
    ok = logi_sink_proc:register_grandchildren_sup(ParentSup, self()),
    Child = #{id => sink_sup, start => {logi_sink_sup, start_link, []}, restart => temporary, type => supervisor},
    {ok, {#{strategy => simple_one_for_one}, [Child]}}.
