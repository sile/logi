%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_backend_manager_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/0,
         start_manager/1,
         stop_manager/1,
         which_managers/0]).

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
-spec start_manager(logi:backend_manager_id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_manager(ManagerId) ->
    supervisor:start_child(?MODULE, [ManagerId]).

%% @doc TODO
-spec stop_manager(logi:backend_manager_id()) -> ok.
stop_manager(ManagerId) ->
    logi_backend_manager:stop(ManagerId).

%% @doc TODO
-spec which_managers() -> [pid()].
which_managers() ->
    [logi_backend_manager:get_id(Pid) || {_, Pid, _, _} <- supervisor:which_children(?MODULE), is_pid(Pid)].

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([]) ->
    Children =
        [
         {logi_backend_manager, {logi_backend_manager, start_link, []}, transient, 3000, worker, [logi_backend_manager]}
        ],
    {ok, {{simple_one_for_one, 5, 10}, Children}}.
