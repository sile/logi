%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_sink_agent_set_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_agent_sup/2]).
-export([stop_agent_sup/2]).

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

-spec start_agent_sup(pid(), supervisor:sup_flags()) -> {ok, pid()} | {error, Reason::term()}.
start_agent_sup(Sup, Flags) ->
    supervisor:start_child(Sup, [Flags]).

-spec stop_agent_sup(pid(), pid()) -> ok.
stop_agent_sup(Sup, AgentSup) ->
    _ = supervisor:terminate_child(Sup, AgentSup),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    Child =
        #{id => agent_sup, start => {logi_sink_agent_sup, start_link, []}, restart => temporary, type => supervisor},
    {ok, {#{strategy => simple_one_for_one}, [Child]}}.
