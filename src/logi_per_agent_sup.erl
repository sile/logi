%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A supervisor of a agent process and ...
%%
%% TODO: doc
%%
%% TODO: code_change test
%%
%% @private
-module(logi_per_agent_sup).

-gen_server(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/2]).
-export([find_child_agent_sup/1]).
-export([which_children/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          agent_spec :: logi_agent:spec(),
          agent_pid :: pid() | undefined,
          listener_pid :: pid(),
          child_agent_sup :: pid() | undefined, % TODO: ensure to be pid (for logi_agent:start_agent/2)
          restart :: logi_restart_strategy:strategy()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a supervisor
%%
-spec start_link(pid(), logi_agent:spec()) -> {ok, pid()} | {error, Reason::term()} | ignore.
start_link(Listener, AgentSpec) ->
    gen_server:start_link(?MODULE, [Listener, AgentSpec], []).

-spec find_child_agent_sup(pid()) -> {ok, pid()} | error.
find_child_agent_sup(SupPid) ->
    gen_server:call(SupPid, find_child_agent_sup).

-spec which_children(pid()) -> [Child] when
      Child :: {atom(), pid() | restarting, worker | supervisor, [module()]}.
which_children(SupPid) ->
    gen_server:call(SupPid, which_children).


%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Listener, AgentSpec]) ->
    _ = process_flag(trap_exit, true),
    {ok, ChildAgentSup} = logi_agent_sup:start_link(),
    case logi_agent:start_agent(AgentSpec, ChildAgentSup) of
        {error, Reason}      ->
            _ = exit(ChildAgentSup, shutdown), % TODO
            {stop, Reason};
        {ignore, ExtraData}  ->
            _ = notify_extra_data(ExtraData, Listener),
            _ = exit(ChildAgentSup, shutdown), % TODO
            ignore;
        {ok, Pid, ExtraData} ->
            _ = monitor(process, Pid),
            _ = report_progress(Pid, AgentSpec),
            State =
                #?STATE{
                    agent_spec = AgentSpec,
                    agent_pid = Pid,
                    listener_pid = Listener,
                    child_agent_sup = ChildAgentSup,
                    restart = logi_agent:get_restart(AgentSpec)
                   },
            _ = notify_agent_up(ExtraData, State),
            {ok, State}
    end.

%% @private
%% A supervisor *must* answer the supervisor:which_children call.
handle_call(which_children, _From, State) ->
    handle_which_children(State);
handle_call(find_child_agent_sup, _From, State) ->
    case State#?STATE.child_agent_sup of
        undefined -> {reply, error, State};
        Pid       -> {reply, {ok, Pid}, State}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _, _, Pid, Reason}, State) ->
    handle_down(Pid, Reason, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_exit(Pid, Reason, State);
handle_info(restart_agent, State) ->
    handle_restart_agent(State);
handle_info(shutdown, State) ->
    handle_shutdown(State);
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_down(pid(), term(), #?STATE{}) -> {noreply, #?STATE{}} | {stop, term(), #?STATE{}}.
handle_down(Pid, Reason, State = #?STATE{agent_pid = Pid}) ->
    handle_exit(Pid, Reason, State);
handle_down(_, _, State) ->
    {noreply, State}.

-spec handle_exit(pid(), term(), #?STATE{}) -> {noreply, #?STATE{}} | {stop, term(), #?STATE{}}.
handle_exit(Pid, Reason, State0 = #?STATE{agent_pid = Pid}) ->
    _ = case Reason of
            nromal        -> ok;
            shutdown      -> ok;
            {shutdown, _} -> ok;
            _             -> report_error(agent_terminated, Reason, State0)
        end,
    ok = notify_agent_down(Reason, State0),
    ok = shutdown_child_agent_sup(State0),
    case schedule_restart(State0) of
        stop         -> {stop, Reason, State0};
        {ok, State1} -> {noreply, State1}
    end;
%% handle_exit(Pid, _, State = #?STATE{child_agent_sup = Pid}) ->
%%     %% TODO: critical(?)
%%     {ok, ChildAgentSup} = logi_agent_sup:start_link(),
%%     {noreply, State#?STATE{child_agent_sup = ChildAgentSup}};
handle_exit(_, Reason, State) ->
    {stop, Reason, State}.

-spec handle_restart_agent(#?STATE{}) -> {noreply, #?STATE{}} | {stop, term(), #?STATE{}}.
handle_restart_agent(State0) ->
    case logi_agent:start_agent(State0#?STATE.agent_spec, State0#?STATE.child_agent_sup) of
        {error, Reason} ->
            case schedule_restart(State0) of
                stop         -> {stop, Reason, State0};
                {ok, State1} -> {noreply, State1}
            end;
        {ok, Pid, ExtraData} ->
            _ = monitor(process, Pid),
            State1 = State0#?STATE{agent_pid = Pid},
            _ = report_progress(Pid, State1#?STATE.agent_spec),
            _ = notify_agent_up(ExtraData, State1),
            {noreply, State1}
    end.

-spec handle_shutdown(#?STATE{}) -> {noreply, #?STATE{}} | {stop, shutdown, #?STATE{}}.
handle_shutdown(State = #?STATE{agent_pid = undefined}) ->
    {stop, shutdown, State};
handle_shutdown(State) ->
    ok = shutdown_child_agent_sup(State),
    ok = case logi_agent:get_shutdown(State#?STATE.agent_spec) of
             undefined -> ok;
             Shutdown  -> shutdown_agent(State, Shutdown)
         end,
    {noreply, State#?STATE{restart = logi_builtin_restart_strategy_stop:new()}}.

-spec handle_which_children(#?STATE{}) -> {reply, Children, #?STATE{}} when
      Children :: [Child],
      Child :: {atom(), pid() | restarting, woker | supervisor, [module()]}.
handle_which_children(State = #?STATE{agent_spec = AgentSpec, agent_pid = AgentPid, child_agent_sup = ChildAgentSup}) ->
    Child = fun (Pid) when is_pid(Pid) -> Pid;
                (_)                    -> restarting
            end,
    Children =
        case logi_agent:get_start(AgentSpec) of
            undefined ->
                [
                 {child_agent_sup, Child(ChildAgentSup), supervisor, [logi_agent_sup]} % TODO: delete
                ];
        {AgentMod, _, _} ->
                [
                 {agent, Child(AgentPid), worker, [AgentMod]},
                 {child_agent_sup, Child(ChildAgentSup), supervisor, [logi_agent_sup]}
                ]
        end,
    {reply, Children, State}.

-spec shutdown_agent(#?STATE{}, logi_agent:shutdown()) -> ok.
shutdown_agent(#?STATE{agent_pid = Pid}, brutal_kill) ->
    _ = exit(Pid, kill),
    ok;
shutdown_agent(#?STATE{agent_pid = Pid}, Shutdown) ->
    _ = exit(Pid, shutdown),
    case Shutdown of
        infinity -> ok;
        _        ->
            _ = timer:apply_after(Shutdown, erlang, exit, [Pid, kill]),
            ok
    end.

-spec shutdown_child_agent_sup(#?STATE{}) -> ok.
shutdown_child_agent_sup(#?STATE{child_agent_sup = undefined}) ->
    ok;
shutdown_child_agent_sup(#?STATE{child_agent_sup = Pid}) ->
    %% TODO: more correct handling
    _ = exit(Pid, shutdown),
    %% _ = unlink(Pid),
    %% receive {'EXIT', Pid, _} -> ok after 0 -> ok end,
    receive {'EXIT', Pid, _} -> ok end, % maybe block infinity
    ok.

-spec schedule_restart(#?STATE{}) -> {ok, #?STATE{}} | stop.
schedule_restart(State) ->
    case logi_restart_strategy:next(State#?STATE.restart) of
        stop                    -> stop;
        {ok, infinity, Restart} ->
            {ok, State#?STATE{restart = Restart, agent_pid = undefined, child_agent_sup = undefined}};
        {ok, NextTime, Restart} ->
            _ = erlang:send_after(NextTime, self(), restart_agent),
            {ok, State#?STATE{restart = Restart, agent_pid = undefined, child_agent_sup = undefined}}
    end.

%% TODO: support other resports
-spec report_progress(pid(), logi_agent:spec()) -> ok.
report_progress(Pid, AgentSpec) ->
    SupName = {self(), ?MODULE},
    Progress = [{supervisor, SupName},
                {started, [{pid, Pid}, {agent_spec, AgentSpec}]}],
    error_logger:info_report(progress, Progress).

-spec report_error(atom(), term(), #?STATE{}) -> ok.
report_error(Error, Reason, #?STATE{agent_pid = Pid, agent_spec = AgentSpec}) ->
    SupName = {self(), ?MODULE},
    ErrorMsg = [{supervisor, SupName},
                {errorContext, Error},
                {reason, Reason},
                {offender, [{pid, Pid}, {agent_spec, AgentSpec}]}],
    error_logger:error_report(supervisor_report, ErrorMsg).

-spec notify_extra_data(logi_sink:extra_data(), pid()) -> ok.
notify_extra_data(ExtraData, ListenerPid) ->
    _ = ListenerPid ! {'AGENT_EXTRA_DATA', ExtraData},
    ok.

-spec notify_agent_up(logi_sink:extra_data(), #?STATE{}) -> ok.
notify_agent_up(ExtraData, #?STATE{listener_pid = ListenerPid, agent_pid = AgentPid}) ->
    _ = ListenerPid ! {'AGENT_UP', self(), AgentPid, ExtraData},
    ok.

-spec notify_agent_down(term(), #?STATE{}) -> ok.
notify_agent_down(Reason, #?STATE{listener_pid = Listener, agent_pid = Agent}) ->
    _ = Listener ! {'AGENT_DOWN', self(), Agent, Reason},
    ok.
