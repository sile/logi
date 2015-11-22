-module(logi_sink_factory).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2]).
-export([is_factory/1]).
-export([is_callback_module/1]).
-export([get_module/1, get_state/1]).
-export([instantiate/2]).
-export([instantiate_if_needed/2]).

-export_type([factory/0]).
-export_type([state/0]).
-export_type([callback_module/0]).
-export_type([start_child_arg/0]).
-export_type([start_child_resutl/0]).
-export_type([instantiate_result/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback instantiate(state()) -> instantiate_result().

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records &　Types
%%----------------------------------------------------------------------------------------------------------------------
-define(FACTORY, ?MODULE).

-record(?FACTORY,
        {
          module :: callback_module(),
          state  :: state()
        }).

-opaque factory() :: #?FACTORY{}.

-type state() :: term().

-type callback_module() :: atom().

-type start_child_arg() :: supervisor:child_spec() | [term()].

-type start_child_resutl() :: {ok, pid(), logi_sink:instance()}
                            | {error, Reason::term()}.

-type instantiate_result() :: {ok, logi_sink:instance()}
                            | {start_child, start_child_arg()}
                            | {error, term()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(callback_module(), state()) -> factory().
new(Module, State) ->
    _ = is_callback_module(Module) orelse error(badarg, [Module, State]),
    #?FACTORY{module = Module, state = State}.

-spec is_factory(factory() | term()) -> boolean().
is_factory(X) -> is_record(X, ?FACTORY).

-spec is_callback_module(callback_module() | term()) -> boolean().
is_callback_module(X) -> is_atom(X) andalso logi_utils:function_exported(X, instantiate, 1).

-spec get_module(factory()) -> callback_module().
get_module(#?FACTORY{module = Module}) -> Module.

-spec get_state(factory()) -> state().
get_state(#?FACTORY{state = State}) -> State.

-spec instantiate(factory(), Supervisor) -> {ok, logi_sink:instance(), ChildPid, ChildId} | {error, Reason} when
      Supervisor :: pid() | atom() | {global, term()} | {via, module(), term()},
      ChildPid   :: pid(), % owner of the sink instance
      ChildId    :: term(),
      Reason     :: term().
instantiate(Factory = #?FACTORY{module = Module, state = State}, Supervisor) ->
    case Module:instantiate(State) of
        {error, Reason}          -> {error, Reason};
        {start_child, ChildSpec} -> start_child(ChildSpec, Supervisor);
        {ok, Instance}           ->
            _ = logi_sink:is_instance(Instance) orelse error({badresult, {ok, Instance}}, [Factory]),
            {ok, Instance, self(), make_ref()}
    end.

-spec instantiate_if_needed(logi_sink:sink(), Supervisor) ->
                                   {ok, logi_sink:instance(), ChildPid, ChildId} | {error, Reason} when
      Supervisor :: pid() | atom() | {global, term()} | {via, module(), term()},
      ChildPid   :: undefined | pid(),
      ChildId    :: undefined | term(),
      Reason     :: term().
instantiate_if_needed(Factory = #?FACTORY{}, Supervisor) ->
    instantiate(Factory, Supervisor);
instantiate_if_needed(Instance, Supervisor) ->
    _ = logi_sink:is_instance(Instance) orelse error(badarg, [Instance, Supervisor]),
    {ok, Instance, self(), make_ref()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Factory
%%----------------------------------------------------------------------------------------------------------------------
-spec start_child(start_child_arg(), pid()) -> {ok, logi_sink:instance(), pid(), term()} | {error, Reason::term()}.
start_child(ChildSpec, Supervisor) ->
    Id = fun (_, #{id := Id})         -> Id;
             (_, {Id, _, _, _, _, _}) -> Id;
             (Pid, _)                 -> Pid
         end,
    case supervisor:start_child(Supervisor, ChildSpec) of
        {error, Reason}     -> {error, Reason};
        {ok, Pid, Instance} ->
            _ = logi_sink:is_instance(Instance) orelse error({badresult, {ok, Pid, Instance}}, [ChildSpec, Supervisor]),
            {ok, Instance, Pid, Id(Pid, ChildSpec)};
        {ok, Pid} ->
            _ = supervisor:terminate_child(Supervisor, Id(Pid, ChildSpec)),
            _ = supervisor:delete_child(Supervisor, Id(Pid, ChildSpec)),
            error({badresult, {ok, Pid}}, [ChildSpec, Supervisor])
    end.
