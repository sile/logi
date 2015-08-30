%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The supervisor for `logi_logger' processes
%% @private
-module(logi_logger_sup).

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

%% @doc Starts a new logger process
-spec start_child(logi:logger_id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_child(LoggerId) ->
    Child = #{id => LoggerId, start => {logi_logger, start_link, [LoggerId]}},
    supervisor:start_child(?MODULE, Child).

%% @doc Stops a logger process which name is `LoggerId'
-spec stop_child(logi:logger_id()) -> ok.
stop_child(LoggerId) ->
    _ = supervisor:terminate_child(?MODULE, LoggerId),
    _ = supervisor:delete_child(?MODULE, LoggerId),
    ok.

%% @doc Returns a logger list
-spec which_children() -> [logi:logger_id()].
which_children() ->
    [Id || {Id, _, _, _} <- supervisor:which_children(?MODULE)].

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    {ok, {#{}, []}}.
