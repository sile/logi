%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc supervisor for logi_backend_manager processes
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
%% @doc Starts a supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a new backend manager process
-spec start_manager(logi:logger()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_manager(ManagerId) ->
    supervisor:start_child(?MODULE, [ManagerId]).

%% @doc Terminates a backend manager process which name is `ManagerId'
-spec stop_manager(logi:logger()) -> ok.
stop_manager(ManagerId) ->
    case whereis(ManagerId) of
        undefined -> ok;
        Pid       -> _ = supervisor:terminate_child(?MODULE, Pid), ok
    end.

%% @doc Returns a manager list
-spec which_managers() -> [ManagerId::logi:logger()].
which_managers() ->
    [logi_backend_manager:get_id(Pid) || {_, Pid, _, _} <- supervisor:which_children(?MODULE), is_pid(Pid)].

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    Children =
        [
         {logi_backend_manager, {logi_backend_manager, start_link, []}, permanent, 3000, worker, [logi_backend_manager]}
        ],
    {ok, {{simple_one_for_one, 5, 10}, Children}}.
