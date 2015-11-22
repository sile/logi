%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_per_channel_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([get_sink_owner_sup/1]).

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
-spec get_sink_owner_sup(pid()) -> pid().
get_sink_owner_sup(SupPid) ->
    [OwnerSup] = [Pid || {sink_owner_sup, Pid, _, _} <- supervisor:which_children(SupPid), is_pid(Pid)],
    OwnerSup.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Channel]) ->
    Children =
        [
         #{id => sink_owner_sup, start => {logi_sink_owner_sup, start_link, []}, type => supervisor},
         #{id => channel, start => {logi_channel, start_link, [Channel]}}
        ],
    {ok, {#{strategy => one_for_all}, Children}}.
