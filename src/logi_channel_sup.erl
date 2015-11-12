%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A supervisor of a `logi_channel' process and a `logi_agent_sup' process
%% @private
-module(logi_channel_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([get_agent_list_sup/1]).

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

%% TODO
get_agent_list_sup(SupPid) ->
    case [Pid || {agent_list_sup, Pid, _, _} <- supervisor:which_children(SupPid), is_pid(Pid)] of
        []    -> undefined;
        [Pid] -> Pid
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Channel]) ->
    Children =
        [
         #{id => channel, start => {logi_channel, start_link, [Channel, self()]}},
         #{id => agent_list_sup, start => {logi_agent_list_sup, start_link, []}, type => supervisor}
        ],
    {ok, {#{strategy => rest_for_one}, Children}}.
