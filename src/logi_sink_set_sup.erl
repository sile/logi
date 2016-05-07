%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Supervisor for logi_sink_sup processes
%% @private
%% @end
-module(logi_sink_set_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_child/3]).
-export([stop_child/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link(?MODULE, [self()]).

%% @doc Starts a new child (i.e., logi_sink_sup process)
-spec start_child(pid(), logi_sink:id(), supervisor:sup_flags()) ->
                         {ok, logi_sink_proc:sink_sup()} | {error, Reason::term()}.
start_child(Sup, SinkId, Flags) ->
    supervisor:start_child(Sup, [SinkId, Flags]).

%% @doc Stops the child
-spec stop_child(pid(), logi_sink_proc:sink_sup()) -> ok.
stop_child(Sup, SinkSup) ->
    _ = supervisor:terminate_child(Sup, SinkSup),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([ParentSup]) ->
    ok = logi_sink_proc:register_grandchildren_sup(ParentSup, self()),
    Child = #{id => sink_sup, start => {logi_sink_sup, start_link, []}, restart => temporary, type => supervisor},
    {ok, {#{strategy => simple_one_for_one}, [Child]}}.
