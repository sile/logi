%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_channel_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([get_agent_set_sup/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a supervisor
-spec start_link(logi_channel:id()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Channel) ->
    supervisor:start_link(?MODULE, [Channel]).

%% TODO: doc
-spec get_agent_set_sup(pid()) -> logi_sink_agent:agent_set_sup().
get_agent_set_sup(Sup) ->
    [AgentSetSup] = [Pid || {agent_set_sup, Pid, _, _} <- supervisor:which_children(Sup), is_pid(Pid)],
    AgentSetSup.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Channel]) ->
    Children =
        [
         #{id => agent_set_sup, start => {logi_sink_agent_set_sup, start_link, []}, type => supervisor},
         #{id => channel, start => {logi_channel, start_link, [Channel]}}
        ],
    {ok, {#{strategy => one_for_all}, Children}}.
