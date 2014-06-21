%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc バックエンドマネージャ用のスーパバイザ
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

%% @doc 新しいマネージャを起動する
-spec start_manager(logi:backend_manager()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_manager(ManagerId) ->
    supervisor:start_child(?MODULE, [ManagerId]).

%% @doc マネージャを停止する
-spec stop_manager(logi:backend_manager()) -> ok.
stop_manager(ManagerId) ->
    logi_backend_manager:stop(ManagerId).

%% @doc マネージャ一覧を取得する
-spec which_managers() -> [logi:backend_manager()].
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
