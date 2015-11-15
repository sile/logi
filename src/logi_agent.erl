%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
%% TODO: logi_agent_handler (for parent sink implementation modules)
-module(logi_agent).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/3]).
-export([new_external/3]).
-export([new_opaque/1]).
-export([is_spec/1]).

-export([get_monitor/1]).

-export([start_agent_if_need/2]).
-export([stop_agent_if_need/2]).
-export([handle_down/3]).

-export_type([agent/0]).
-export_type([spec/0]).

-export_type([mfargs/0]).
-export_type([start/0]).
-export_type([proc_ref/0]).
-export_type([shutdown/0]).

-export_type([start_result/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(AGENT, ?MODULE).

-record(?AGENT,
        {
          sup_pid   :: undefined | pid(),
          agent_pid :: pid(),
          monitor   :: reference(),
          restart   :: undefined | logi_restart_strategy:strategy(),
          timer = make_ref() :: reference()
        }).

-opaque agent() :: #?AGENT{}.

-record(agent_spec,
        {
          start    :: mfargs(),
          restart  :: logi_restart_strategy:strategy(),
          shutdown :: timeout() | brutal_kill
        }).

-record(external_agent,
        {
          agent_ref  :: proc_ref(),
          restart    :: logi_restart_strategy:strategy(),
          extra_data :: logi_sink:extra_data()
        }).

-record(opaque_agent,
        {
          extra_data :: logi_sink:extra_data()
        }).

-opaque spec() :: #agent_spec{} | #external_agent{} | #opaque_agent{}.

-type mfargs() :: {Module::module(), Function::atom(), Args::[term()]}.

-type proc_ref() :: pid()
                  | atom()
                  | {global, term()}
                  | {via, module(), term()}.

-type start() :: mfargs()
               | {external, proc_ref()}
               | {external, proc_ref(), logi_sink:extra_data()}
               | ignore
               | {ignore, logi_sink:extra_data()}.

-type start_result() :: {ok, pid()}
                      | {ok, pid(), logi_sink:extra_data()}
                      | {error, term()}.

-type shutdown() :: timeout() | brutal_kill.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(mfargs(), logi_restart_strategy:strategy(), shutdown()) -> spec().
new(Start, Restart, Shutdown) ->
    _ = is_mfargs(Start) orelse error(badarg, [Start, Restart, Shutdown]),
    _ = logi_restart_strategy:is_strategy(Restart) orelse error(badarg, [Start, Restart, Shutdown]),
    _ = is_shutdown(Shutdown) orelse error(badarg, [Start, Restart, Shutdown]),
    #agent_spec{start = Start, restart = Restart, shutdown = Shutdown}.

-spec new_external(proc_ref(), logi_restart_strategy:strategy(), logi_sink:extra_data()) -> spec().
new_external(AgentRef, Restart, ExtraData) ->
    _ = is_proc_ref(AgentRef) orelse error(badarg, [AgentRef, Restart, ExtraData]),
    _ = logi_restart_strategy:is_strategy(Restart) orelse error(badarg, [AgentRef, Restart, ExtraData]),
    #external_agent{agent_ref = AgentRef, restart = Restart, extra_data = ExtraData}.

-spec new_opaque(logi_sink:extra_data()) -> spec().
new_opaque(ExtraData) ->
    #opaque_agent{extra_data = ExtraData}.

-spec is_spec(X :: (spec() | term())) -> boolean().
is_spec(X) -> is_record(X, agent_spec) orelse is_record(X, external_agent) orelse is_record(X, opaque_agent).

-spec get_monitor(agent()) -> reference().
get_monitor(#?AGENT{monitor = Monitor}) ->
    Monitor.

-spec start_agent_if_need(spec(), pid()) -> {ok, agent(), logi_sink:extra_data()} | {error, Reason::term()}.
start_agent_if_need(#opaque_agent{extra_data = ExtraData}, _) ->
    DummyPid = self(),
    Monitor = monitor(process, DummyPid),
    {ok, #?AGENT{agent_pid = DummyPid, monitor = Monitor}, ExtraData};
start_agent_if_need(#external_agent{agent_ref = AgentRef, restart = Restart, extra_data = ExtraData}, _) ->
    case where(AgentRef) of
        undefined -> {error, {external_agent_not_found, AgentRef}};
        AgentPid  ->
            Monitor = monitor(process, AgentPid),
            {ok, #?AGENT{agent_pid = AgentPid, monitor = Monitor, restart = Restart}, ExtraData}
    end;
start_agent_if_need(#agent_spec{start = Start, shutdown = Shutdown, restart = Restart}, AgentListSup) ->
    case logi_agent_list_sup:start_agent(AgentListSup, Start, Shutdown) of
        {error, Reason}                   -> {error, Reason};
        {ok, SupPid, AgentPid, ExtraData} ->
            Monitor = monitor(process, AgentPid),
            Agent = #?AGENT{sup_pid = SupPid, agent_pid = AgentPid, monitor = Monitor, restart = Restart},
            {ok, Agent, ExtraData}
    end.

-spec stop_agent_if_need(agent(), pid()) -> agent().
stop_agent_if_need(Agent = #?AGENT{sup_pid = undefined, monitor = Monitor}, _) ->
    _ = demonitor(Monitor, [flush]),
    _ = erlang:cancel_timer(Agent#?AGENT.timer),
    Agent;
stop_agent_if_need(Agent = #?AGENT{sup_pid = SupPid, monitor = Monitor}, AgentListSup) ->
    ok = logi_agent_list_sup:stop_agent(AgentListSup, SupPid),
    _ = erlang:cancel_timer(Agent#?AGENT.timer),
    _ = demonitor(Monitor, [flush]),
    Agent#?AGENT{sup_pid = SupPid}.

-spec handle_down(agent(), logi_sink:id(), pid()) -> {retry_after, agent()} | {uninstall, Reason::term()}.
handle_down(Agent0, SinkId, AgentListSup) ->
    Agent1 = logi_agent:stop_agent_if_need(Agent0, AgentListSup),
    case logi_restart_strategy:next(Agent0#?AGENT.restart) of
        {uninstall_sink, Reason} -> {uninstall, Reason};
        {ok, Timeout, Restart} -> % TODO: handle infinity
            RestartTimer = erlang:send_after(Timeout, self(), {restart, SinkId}),
            Agent2 = Agent1#?AGENT{restart = Restart, timer = RestartTimer},
            {retry_after, Agent2}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_mfargs(mfargs() | term()) -> boolean().
is_mfargs({Module, Function, Args}) -> is_atom(Module) andalso is_atom(Function) andalso is_list(Args);
is_mfargs(_)                        -> false.

-spec is_shutdown(timeout() | brutal_kill | term()) -> boolean().
is_shutdown(infinity)    -> true;
is_shutdown(brutal_kill) -> true;
is_shutdown(Timeout)     -> is_integer(Timeout) andalso Timeout >= 0.

-spec is_proc_ref(proc_ref() | term()) -> boolean().
is_proc_ref({global, _})      -> true;
is_proc_ref({via, Module, _}) -> is_atom(Module);
is_proc_ref(X)                -> is_pid(X) orelse is_atom(X).

-spec where(proc_ref()) -> pid() | undefined.
where(X) when is_pid(X)  -> X;
where(X) when is_atom(X) -> whereis(X);
where({global, X})       -> global:whereis_name(X);
where({via, Module, X})  -> Module:whereis_name(X).
