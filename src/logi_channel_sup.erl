%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The root supervisor of a supervision tree which is constituted by channel and agent processes
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
start_child(Channel) ->
    Child = #{id => Channel, start => {logi_per_channel_sup, start_link, [Channel]}, type => supervisor},
    supervisor:start_child(?MODULE, Child).

%% @doc Stops a channel process which name is `Channel'
-spec stop_child(logi_channel:id()) -> ok.
stop_child(Channel) ->
    _ = supervisor:terminate_child(?MODULE, Channel),
    _ = supervisor:delete_child(?MODULE, Channel),
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
    Default = logi_channel:default_channel(),
    Child = #{id => Default, start => {logi_per_channel_sup, start_link, [Default]}, type => supervisor},
    {ok, {#{}, [Child]}}.
