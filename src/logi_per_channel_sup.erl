%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A supervisor of a `logi_channel' process and a `logi_agent_sup' process
%% @private
-module(logi_per_channel_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([find_child_agent_sup/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a supervisor for the channel `Channel'
-spec start_link(logi_channel:id()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Channel) ->
    supervisor:start_link(?MODULE, [Channel]).

%% @doc TODO
-spec find_child_agent_sup(pid()) -> {ok, pid()} | error.
find_child_agent_sup(SupPid) ->
    case [Pid || {child_agent_sup, Pid, _, _} <- supervisor:which_children(SupPid), is_pid(Pid)] of
        []    -> error;
        [Pid] -> {ok, Pid}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Channel]) ->
    Children =
        [
         #{id => child_agent_sup, start => {logi_agent_sup, start_link, []}, type => supervisor},
         #{id => channel, start => {logi_channel, start_link, [Channel, self()]}}
        ],
    {ok, {#{strategy => one_for_all}, Children}}.
