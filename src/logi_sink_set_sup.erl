%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_sink_set_sup). % TODO: rename

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_sink_sup/2]).
-export([stop_sink_sup/2]).
-export([get_current_process/1]).

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

-spec start_sink_sup(pid(), supervisor:sup_flags()) -> {ok, pid()} | {error, Reason::term()}.
start_sink_sup(Sup, Flags) ->
    supervisor:start_child(Sup, [Flags]).

-spec stop_sink_sup(pid(), pid()) -> ok.
stop_sink_sup(Sup, SinkSup) ->
    _ = supervisor:terminate_child(Sup, SinkSup),
    ok.

-spec get_current_process(pid()) -> pid().
get_current_process(ParentSup) ->
    {_, Entries} = process_info(ParentSup, dictionary),
    case lists:keyfind({?MODULE, 'CURRENT_PID'}, 1, Entries) of
        false    -> error(current_sink_set_sup_is_not_found, [ParentSup]);
        {_, Pid} -> Pid
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([ParentSup]) ->
    ok = logi_name_server:register_name({ParentSup, grandchildren_sup}, self()),
    %% TODO: one_for_one and id=logi_sink:get_id(...)
    Child =
        #{id => sink_sup, start => {logi_sink_sup, start_link, []}, restart => temporary, type => supervisor},
    {ok, {#{strategy => simple_one_for_one}, [Child]}}.
