-module(logi_name_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).

-export_type([name/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(entry,
        {
          name    :: name(),
          pid     :: pid(),
          monitor :: reference()
        }).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          names = #{} :: #{name() => #entry{}},
          procs = #{} :: #{pid() => #entry{}}
        }).

-type name() :: term().

-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_name(name(), pid()) -> ok.
register_name(Name, Pid) ->
    gen_server:cast(?MODULE, {register_name, {Name, Pid}}).

-spec unregister_name(name()) -> ok.
unregister_name(Name) ->
    gen_server:cast(?MODULE, {unregister_name, Name}).

-spec whereis_name(name()) -> pid() | undefined.
whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis_name, Name}).

%% @private
init([]) ->
    State = #?STATE{},
    {ok, State}.

%% @private
handle_call({whereis_name, Arg}, _From, State) ->
    handle_whereis_name(Arg, State);
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({register_name, Arg}, State) ->
    handle_register_name(Arg, State);
handle_cast({unregister_name, Arg}, State) ->
    handle_unregister_name(Arg, State);
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _, _, Pid, _}, State) ->
    handle_down(Pid, State);
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_register_name({name(), pid()}, #?STATE{}) -> {noreply, #?STATE{}}.
handle_register_name({Name, Pid}, State0) ->
    case maps:find(Name, State0#?STATE.names) of
        {ok, #entry{pid = Pid}} -> {noreply, State0};
        _                       ->
            {_, State1} = handle_unregister_name(Name, State0),
            {_, State2} = handle_down(Pid, State1),

            Monitor = monitor(process, Pid),
            Entry = #entry{name = Name, pid = Pid, monitor = Monitor},
            Names = maps:put(Name, Entry, State2#?STATE.names),
            Procs = maps:put(Pid, Entry, State2#?STATE.procs),
            {noreply, State2#?STATE{names = Names, procs = Procs}}
    end.

-spec handle_unregister_name(name(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_unregister_name(Name, State) ->
    case maps:find(Name, State#?STATE.names) of
        error   -> {noreply, State};
        {ok, E} ->
            _ = demonitor(E#entry.monitor, [flush]),
            Names = maps:remove(Name, State#?STATE.names),
            Procs = maps:remove(E#entry.pid, State#?STATE.procs),
            {noreply, State#?STATE{names = Names, procs = Procs}}
    end.

-spec handle_whereis_name(name(), #?STATE{}) -> {reply, pid() | undefined, #?STATE{}}.
handle_whereis_name(Name, State) ->
    case maps:find(Name, State#?STATE.names) of
        error   -> {reply, undefined, State};
        {ok, E} -> {reply, E#entry.pid, State}
    end.

-spec handle_down(pid(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Pid, State) ->
    case maps:find(Pid, State#?STATE.procs) of
        error   -> {noreply, State};
        {ok, E} -> handle_unregister_name(E#entry.name, State)
    end.
