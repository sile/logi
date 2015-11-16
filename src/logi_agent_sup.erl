%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A supervisor of agent processes
%%
%% TODO: doc
%%
%% TODO: => logi_agent_set_sup
%%
%% @private
-module(logi_agent_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_agent/2]).
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
-spec start_agent(pid(), logi_agent:spec()) -> {ok, pid(), logi_sink:extra_data()} | {error, Reason::term()}.
start_agent(SupPid, AgentSpec) ->
    case supervisor:start_child(SupPid, [self(), AgentSpec]) of
        {error, Reason}   -> {error, Reason};
        ignore            ->
            receive {'AGENT_EXTRA_DATA', ExtraData} -> ok end,
            {ok, self(), ExtraData}; % Returns dummy pid()
        {ok, AgentSupPid} ->
            receive {'AGENT_UP', AgentSupPid, _, ExtraData} -> ok end,
            {ok, AgentSupPid, ExtraData}
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
    Child = #{id => logi_per_agent_sup, start => {logi_per_agent_sup, start_link, []}, restart => temporary, type => supervisor},
    {ok, {#{strategy => simple_one_for_one}, [Child]}}.
