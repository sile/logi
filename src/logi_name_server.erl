%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Application Internal Process Name Server
%% @private
%% @end
-module(logi_name_server).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).

-export_type([name/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
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
%% Process Name

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts the name server process
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Locally assocates the name `Name' with a pid `Pid'.
%%
%% The function returns `yes' if successful, `no' if it failes.
%% For example, `no' is returned if an attempt is made to register an already registered process or
%% to register a process with a name that is already in use.
-spec register_name(name(), pid()) -> yes | no.
register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register_name, {Name, Pid}}).

%% @doc Removes the locally registered name `Name'
-spec unregister_name(name()) -> ok.
unregister_name(Name) ->
    gen_server:cast(?MODULE, {unregister_name, Name}).

%% @doc Returns the pid with the locally registered name `Name'
%%
%% Returns `undefined' if the name is not locally registered.
-spec whereis_name(name()) -> pid() | undefined.
whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis_name, Name}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    State = #?STATE{},
    {ok, State}.

%% @private
handle_call({register_name, Arg}, _From, State) ->
    handle_register_name(Arg, State);
handle_call({whereis_name, Arg}, _From, State) ->
    handle_whereis_name(Arg, State);
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
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

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_register_name({name(), pid()}, #?STATE{}) -> {reply, yes|no, #?STATE{}}.
handle_register_name({Name, Pid}, State) ->
    case maps:is_key(Name, State#?STATE.names) orelse maps:is_key(Pid, State#?STATE.procs) of
        true  -> {reply, no, State};
        false ->
            Monitor = monitor(process, Pid),
            Entry = #entry{name = Name, pid = Pid, monitor = Monitor},
            Names = maps:put(Name, Entry, State#?STATE.names),
            Procs = maps:put(Pid, Entry, State#?STATE.procs),
            {reply, yes, State#?STATE{names = Names, procs = Procs}}
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
