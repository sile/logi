%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Supervisor for a logi_channel process and a supervisor of its children
%% @private
%% @end
-module(logi_channel_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).

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

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Channel]) ->
    Children =
        [
         #{id => sink_set_sup, start => {logi_sink_set_sup, start_link, []}, type => supervisor},
         #{id => channel, start => {logi_channel, start_link, [Channel]}}
        ],
    {ok, {#{strategy => one_for_all}, Children}}.
