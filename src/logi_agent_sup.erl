%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A supervisor of agent processes
%%
%% TODO: doc
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
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(POP_FROM_MSG_QUEUE(Pattern),
        receive Pattern -> ok after 0 -> Pattern = error(invalid_msg_queue_contents) end).

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
        {error, Reason} -> {error, Reason};
        {ok, undefined} ->
            %% TODO: errorの場合でも、receiveしておかないとゴミが残りそう (reference()を渡す方式に変えた方がよい)
            ?POP_FROM_MSG_QUEUE({'AGENT_EXTRA_DATA', ExtraData}),
            {ok, self(), ExtraData}; % Returns dummy pid()
        {ok, AgentSupPid} ->
            ?POP_FROM_MSG_QUEUE({'AGENT_UP', AgentSupPid, _, ExtraData}),
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
