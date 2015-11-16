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
-export([get_shutdown/1]).
-export([get_restart/1]).
-export([get_start/1]).

-export([start_agent_if_need/1]).

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
                  | {via, module(), term()}
                  | {fun ((term()) -> pid() | undefined), term()}.

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

-spec get_shutdown(spec()) -> shutdown() | undefined.
get_shutdown(#agent_spec{shutdown = Shutdown}) -> Shutdown;
get_shutdown(_)                                -> undefined.

-spec get_restart(spec()) -> logi_restart_strategy:strategy().
get_restart(#agent_spec{restart = Restart})     -> Restart;
get_restart(#external_agent{restart = Restart}) -> Restart;
get_restart(#opaque_agent{})                    -> logi_builtin_restart_strategy_stop:new().

-spec get_start(spec()) -> mfargs() | undefined.
get_start(#agent_spec{start = Start}) -> Start;
get_start(_)                          -> undefined.

-spec start_agent_if_need(spec()) -> {ok, pid(), logi_sink:extra_data()} | {error, Reason::term()} | {ignore, logi_sink:extra_data()}.
start_agent_if_need(#opaque_agent{extra_data = ExtraData}) ->
    {ignore, ExtraData};
start_agent_if_need(#external_agent{agent_ref = AgentRef, extra_data = ExtraData}) ->
    case where(AgentRef) of
        undefined -> {error, {external_agent_not_found, AgentRef}};
        AgentPid  -> {ok, AgentPid, ExtraData}
    end;
start_agent_if_need(#agent_spec{start = {M, F, Args}}) ->
    case (catch apply(M, F, Args)) of
        {ok, Pid}            -> {ok, Pid, undefined};
        {ok, Pid, ExtraData} -> {ok, Pid, ExtraData};
        {error, Reason}      -> {error, Reason};
        Other                -> {error, Other}
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
is_proc_ref({ResolveFn, _})   -> is_function(ResolveFn, 1);
is_proc_ref(X)                -> is_pid(X) orelse is_atom(X).

-spec where(proc_ref()) -> pid() | undefined.
where(X) when is_pid(X)  -> X;
where(X) when is_atom(X) -> whereis(X);
where({global, X})       -> global:whereis_name(X);
where({via, Module, X})  -> Module:whereis_name(X);
where({ResolveFn, X})    -> ResolveFn(X).
