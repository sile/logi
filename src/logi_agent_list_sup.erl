%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A supervisor of agent processes
%%
%% TODO: doc
%%
%% @private
-module(logi_agent_list_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_agent/3]).
-export([stop_agent/2]).

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
    supervisor:start_link(?MODULE, []).

%% @doc Starts a new agent
-spec start_agent(pid(), logi_agent:mfargs(), logi_agent:shutdown()) ->
                         {ok, AgentSupPid::pid(), AgentPid::pid(), logi_sink:extra_data()} | {error, Reason::term()}.
start_agent(SupPid, Start, Shutdown) ->
    case supervisor:start_child(SupPid, []) of
        {error, Reason}   -> {error, Reason};
        {ok, AgentSupPid} ->
            case logi_agent_sup:start_agent(AgentSupPid, Start, Shutdown) of
                {ok, AgentPid}        -> {ok, AgentSupPid, AgentPid, undefined};
                {ok, AgentPid, Extra} -> {ok, AgentSupPid, AgentPid, Extra};
                {error, Reason}       ->
                    ok = stop_agent(SupPid, AgentSupPid),
                    {error, Reason}
            end
    end.

%% @doc Stops the agent which is supervised by `AgentSupPid'
-spec stop_agent(pid(), pid()) -> ok.
stop_agent(SupPid, AgentSupPid) ->
    _ = supervisor:terminate_child(SupPid, AgentSupPid),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    Child = #{id => agent_sup, start => {logi_agent_sup, start_link, []}, type => supervisor},
    {ok, {#{strategy => simple_one_for_one}, [Child]}}.
