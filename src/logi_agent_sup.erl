%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A supervisor of a agent process and ...
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
-export([start_agent/3]).
-export([start_nephew_agent/3]).
-export([stop_nephew_agent/2]).

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
                         {ok, pid()} | {ok, pid(), logi_sink:extra_data()} | {error, Reason::term()}.
start_agent(SupPid, {M, F, Args}, Shutdown) ->
    Start = {M, F, [SupPid | Args]},
    Child = #{id => agent, start => Start, shutdown => Shutdown},
    supervisor:start_child(SupPid, Child).

%% TODO: doc
-spec start_nephew_agent(pid(), logi_agent:mfargs(), logi_agent:shutdown()) ->
                                {ok, AgentSupPid::pid(), AgentPid::pid(), logi_sink:extra_data()} | {error, Reason::term()}.
start_nephew_agent(SupPid, Start, Shutdown) ->
    EnsureNephewSupStarted =
        fun () ->
                Child = #{id => nephew_sup, start => {logi_agent_list_sup, start_link, []}, type => supervisor},
                case supervisor:start_child(SupPid, Child) of
                    {error, {already_started, Pid}} -> {ok, Pid};
                    Other                           -> Other
                end
        end,
    case EnsureNephewSupStarted() of
        {error, Reason}    -> {error, Reason};
        {ok, NephewSupPid} -> logi_agent_list_sup:start_agent(NephewSupPid, Start, Shutdown)
    end.

-spec stop_nephew_agent(pid(), pid()) -> ok.
stop_nephew_agent(SupPid, AgentSupPid) ->
    case [Pid || {nephew_sup, Pid, _, _} <- supervisor:which_children(SupPid), is_pid(Pid)] of
        []             -> ok;
        [NephewSupPid] -> logi_agent_list_sup:stop_agent(NephewSupPid, AgentSupPid)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    {ok, {#{}, []}}.
