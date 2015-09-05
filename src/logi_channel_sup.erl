%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The supervisor for `logi_channel' processes
%% @private
-module(logi_channel_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_child/1, stop_child/1, which_children/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts the supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a new channel process
-spec start_child(logi_channel:id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_child(ChannelId) ->
    Child = {ChannelId, {logi_channel, start_link, [ChannelId]}, permanent, 5000, worker, [logi_channel]},
    supervisor:start_child(?MODULE, Child).

%% @doc Stops a channel process which name is `ChannelId'
-spec stop_child(logi_channel:id()) -> ok.
stop_child(ChannelId) ->
    _ = supervisor:terminate_child(?MODULE, ChannelId),
    _ = supervisor:delete_child(?MODULE, ChannelId),
    ok.

%% @doc Returns a channel list
-spec which_children() -> [logi_channel:id()].
which_children() ->
    [Id || {Id, _, _, _} <- supervisor:which_children(?MODULE)].

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    {ok, {{one_for_one, 1, 5}, []}}.
