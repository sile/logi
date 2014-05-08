%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_event_manager_sup).

-include("logi.hrl").
-behaviour(supervisor).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/0,
         start_event_manager/0, start_event_manager/1,
         stop_event_manager/1,
         which_event_managers/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Starts root supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc TODO
-spec start_event_manager() -> {ok, pid()} | {error, Reason::term()}.
start_event_manager() ->
    supervisor:start_child(?MODULE, []).

%% @doc TODO
-spec start_event_manager(logi:event_manager_name()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_event_manager(ManagerName) ->
    supervisor:start_child(?MODULE, [ManagerName]).

%% @doc TODO
-spec stop_event_manager(logi:event_manager_ref()) -> ok.
stop_event_manager(ManagerRef) ->
    logi_event_manager:stop(ManagerRef).

%% @doc TODO
-spec which_event_managers() -> [pid()].
which_event_managers() ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(?MODULE), is_pid(Pid)].

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([]) ->
    Children =
        [
         {logi_event_manager, {logi_event_manager, start_link, []}, transient, 3000, worker, [logi_event_manager]}
        ],
    {ok, {{simple_one_for_one, 5, 10}, Children}}.
