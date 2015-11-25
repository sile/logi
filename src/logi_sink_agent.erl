%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_sink_agent).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
%% -export([get_sup_flags/1]).
-export([new/2, new/3]).
-export([is_spec/1]).

-export([start_agent/2, stop_agent/2]).

-export([suspend_sink/2]).
-export([resume_sink/2]).
-export([update_sink/2]).

%% TODO: get_agent_tree

-export_type([spec/0]).
-export_type([callback_module/0]).
-export_type([data/0]).

-export_type([agent_set_sup/0, agent_sup/0]).
-export_type([controller/0]).
-export_type([control_message/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback make_child_spec(controller(), agent_set_sup(), data()) -> supervisor:child_spec().
%% @see make_child_spec/3

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(SPEC, ?MODULE).

-record(?SPEC,
        {
          module    :: callback_module(),
          sup_flags :: supervisor:sup_flags(),
          data      :: data()
        }).

-opaque spec() :: #?SPEC{}.


-type callback_module() :: module().
-type data() :: term().

-type agent_set_sup() :: pid().
-type agent_sup() :: pid().

-opaque controller() :: {pid(), agent_sup()}.

-type control_message() :: {'SINK_CTRL', suspend, agent_sup(), pid(), term()}
                         | {'SINK_CTRL', resume, agent_sup(), pid(), term()}
                         | {'SINK_CTRL', update, agent_sup(), pid(), logi_sink:sink()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(callback_module(), data()) -> spec().
new(Callbacks, Data) ->
    new(Callbacks, Data, #{strategy => one_for_all}).

-spec new(callback_module(), data(), supervisor:sup_flags()) -> spec().
new(CallbackModule, Data, SupFlags) ->
    %% TODO: validate
    #?SPEC{module = CallbackModule, sup_flags = SupFlags, data = Data}.

-spec is_spec(spec() | term()) -> boolean().
is_spec(X) -> is_record(X, ?SPEC).

-spec suspend_sink(controller(), Reason::term()) -> ok.
suspend_sink(Ref = {Pid, _}, Reason) ->
    _ = Pid ! {'SINK_SUSPEND', Ref, self(), Reason},
    ok.

-spec resume_sink(controller(), term()) -> ok.
resume_sink(Ref = {Pid, _}, Reason) ->
    _ = Pid ! {'SINK_RESUME', Ref, self(), Reason},
    ok.

-spec update_sink(controller(), logi_sink:sink()) -> ok.
update_sink(Ref = {Pid, _}, Sink) ->
    _ = logi_sink:is_sink(Sink) orelse error(badarg, [Pid, Sink]),
    _ = Pid ! {'SINK_UPDATE', Ref, self(), Sink},
    ok.

%% @private
-spec start_agent(agent_set_sup(), spec()) -> {ok, logi_sink:sink(), agent_sup()} | {error, term()}.
start_agent(ParentSup, Spec) ->
    SupFlags = Spec#?SPEC.sup_flags,
    case logi_sink_agent_set_sup:start_agent_sup(ParentSup, SupFlags) of
        {error, Reason} -> {error, Reason};
        {ok, AgentSup}  ->
            Controller = {self(), AgentSup},
            try
                ChildrenSup = logi_sink_agent_sup:get_child_agent_set_sup(AgentSup),
                ChildSpec = (Spec#?SPEC.module):make_child_spec(Controller, ChildrenSup, Spec#?SPEC.data),
                case logi_sink_agent_sup:start_agent(AgentSup, ChildSpec) of
                    {error, Reason} ->
                        ok = logi_sink_agent_set_sup:stop_agent_sup(ParentSup, AgentSup),
                        {error, Reason};
                    {ok, _Pid, Sink} ->
                        {ok, Sink, AgentSup}
                end
            catch
                ExClass:ExReason ->
                    ok = logi_sink_agent_set_sup:stop_agent_sup(ParentSup, AgentSup),
                    erlang:raise(ExClass, ExReason, erlang:get_stacktrace())
            end
    end.

%% @private
-spec stop_agent(agent_set_sup(), agent_sup()) -> ok.
stop_agent(ParentSup, AgentSup) ->
    logi_sink_agent_set_sup:stop_agent_sup(ParentSup, AgentSup).
