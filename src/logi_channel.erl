%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Message Channels
%%
%% A channel manages conditional sinks
%%
%% <pre lang="erlang">
%% %%%
%% %%% Example
%% %%%
%%
%% %%
%% %% CREATE
%% %%
%% > ok = logi_channel:create(sample_log).
%% > logi_channel:which_channels().
%% [sample_log,logi_default_log]  % 'logi_default_log' is created automatically when 'logi' application was started
%%
%% %%
%% %% INSTALL SINK
%% %%
%% > Sink = logi_sink:new(logi_builtin_sink_null).
%% > {ok, _} = logi_channel:install_sink(sample_log, Sink).
%% > logi_channel:which_sinks(sample_log).
%% [logi_builtin_sink_null]
%% </pre>
-module(logi_channel).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([default_channel/0]).

-export([create/1]).
-export([delete/1]).
-export([which_channels/0]).

-export([install_sink/2, install_sink/3]).
-export([uninstall_sink/2]).
-export([find_sink/2]).
-export([which_sinks/1]).
-export([set_condition/3]).

-export_type([id/0]).
-export_type([install_sink_option/0,  install_sink_options/0]).
-export_type([install_sink_result/0]).
-export_type([uninstall_sink_result/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([select_sink/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(VALIDATE_AND_GET_CHANNEL_PID(Channel, Args),
        case is_atom(Channel) of
            false -> error(badarg, Args);
            true  -> case whereis(Channel) of
                         undefined -> error({channel_is_not_running, Channel}, Args);
                         ChannelPid -> ChannelPid
                     end
        end).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          id         :: id(),
          table      :: logi_sink_table:table(),
          sinks = [] :: sinks()
        }).

-type sinks() :: [{logi_sink:id(), lifetime_ref(), cancel_lifetime_fun(), logi_sink:sink()}].

-type lifetime_ref() :: undefined | reference().
-type cancel_lifetime_fun() :: fun (() -> any()).

-type id() :: atom().
%% The identifier of a channel

-type install_sink_options() :: [install_sink_option()].

-type install_sink_option() :: {lifetime, timeout() | pid()}
                             | {if_exists, error | ignore | supersede}.
%% `lifetime':
%% - The lifetime of a sink.
%% - When `timeout()' expires or `pid()' exits, the sink will be automatically uninstalled from the channel.
%% - default: `infinity'
%%
%% `if_exists':
%% - The confliction handling policy.
%% - If a sink with the same identifier already exists,
%% &#x20;&#x20;- `error': the function will return an error `{error, {already_installed, ExistingSink}}'.
%% &#x20;&#x20;- `ignore': the new sink will be ignored. Then the function will return `{ok, ExistingSink}'.
%% &#x20;&#x20;- `supersede': the new sink will supersede it. Then the function will return `{ok, OldSink}'.
%% - default: `error'

-type install_sink_result() :: {ok, OldSink :: undefined | logi_sink:sink()}
                             | {error, {already_installed, logi_sink:sink()}}.
%% The result of {@link install_sink/2}.
%%
%% If there does not exist a sink which has the same identifier with a new one,
%% the function will return `{ok, undefined}'.
%%
%% Otherwise the result value depends on the value of the `if_exists' option
%% (see the description of `install_sink_option/0' for details).

-type uninstall_sink_result() :: {ok, logi_sink:sink()} | error.
%% The result of {@link uninstall_sink/2}.
%%
%% The function will return `{ok, Sink}' if the specified sink exists in the channel, `error' otherwise.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc The default channel
%%
%% This channel is created automatically when `logi' application was started.
-spec default_channel() -> id().
default_channel() -> logi:default_logger().

%% @doc Creates a new channel
%%
%% If the channel already exists, nothing happens.
%%
%% If there exists a process or a ETS table with the same name as `Channel', the function will crash.
-spec create(id()) -> ok.
create(Channel) ->
    case logi_channel_sup:start_child(Channel) of
        {ok, _} -> ok;
        _       ->
            %% FIXME: The judgement may be inaccurate
            case lists:member(Channel, which_channels()) of
                true  -> ok;
                false -> error(badarg, [Channel])
            end
    end.

%% @doc Deletes a channel
%%
%% If the channel does not exists, it will be silently ignored.
-spec delete(id()) -> ok.
delete(Channel) when is_atom(Channel) -> logi_channel_sup:stop_child(Channel);
delete(Channel)                       -> error(badarg, [Channel]).

%% @doc Returns a list of all existing channels
-spec which_channels() -> [id()].
which_channels() -> logi_channel_sup:which_children().

%% @equiv install_sink(Channel, Sink, [])
-spec install_sink(id(), logi_sink:sink()) -> install_sink_result().
install_sink(Channel, Sink) -> install_sink(Channel, Sink, []).

%% @doc Installs `Sink' into `Channel'
-spec install_sink(id(), logi_sink:sink(), install_sink_options()) -> install_sink_result().
install_sink(Channel, Sink, Options) ->
    Args = [Channel, Sink, Options],
    _ = logi_sink:is_sink(Sink) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    IfExists = proplists:get_value(if_exists, Options, error),
    Lifetime = proplists:get_value(lifetime, Options, infinity),
    _ = lists:member(IfExists, [error, ignore, supersede]) orelse error(badarg, Args),
    _ = is_valid_lifetime(Lifetime) orelse error(badarg, Args),

    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, Args),
    gen_server:call(Pid, {install_sink, {Sink, Lifetime, IfExists}}).

%% @doc Uninstalls the sink which has the identifier `SinkId' from `Channel'
-spec uninstall_sink(id(), logi_sink:id()) -> uninstall_sink_result().
uninstall_sink(Channel, SinkId) ->
    _ = is_atom(SinkId) orelse error(badarg, [Channel, SinkId]),
    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, [Channel, SinkId]),
    gen_server:call(Pid, {uninstall_sink, SinkId}).

%% @doc Searchs for `SinkId' in `Channel'; returns `{ok, Sink}', or `error' if `SinkId' is not present
-spec find_sink(id(), logi_sink:id()) -> {ok, logi_sink:sink()} | error.
find_sink(Channel, SinkId) ->
    _ = is_atom(SinkId) orelse error(badarg, [Channel, SinkId]),
    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, [Channel, SinkId]),
    gen_server:call(Pid, {find_sink, SinkId}).

%% @doc Returns a list of installed sinks
-spec which_sinks(id()) -> [logi_sink:id()].
which_sinks(Channel) ->
    _ = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, [Channel]),
    logi_sink_table:which_sinks(Channel).

%% @doc Sets the condition of the sink which has the identifier `SinkId' to `Condition'
%%
%% This function will returns `{ok, OldCondition}' if specified sink exists, `error' otherwise.
%% `OldCondition' is the old condition of the sink.
-spec set_condition(id(), logi_sink:id(), logi_sink:condition()) -> {ok, logi_sink:condition()} | error.
set_condition(Channel, SinkId, Condition) ->
    _ = is_atom(SinkId) orelse error(badarg, [Channel, SinkId, Condition]),
    _ = logi_sink:is_condition(Condition) orelse error(badarg, [Channel, SinkId, Condition]),
    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, [Channel, SinkId, Condition]),
    gen_server:call(Pid, {set_condition, {SinkId, Condition}}).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a channel process
%%
%% @private
-spec start_link(id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(Channel) ->
    gen_server:start_link({local, Channel}, ?MODULE, [Channel], []).

%% @doc Selects sinks that meet the condition
%%
%% If the channel does not exist, it will returns an empty list.
%%
%% @private
-spec select_sink(id(), logi:severity(), atom(), module()) -> [Sink] when
      Sink :: {logi_sink:callback_module(), logi_sink:extra_data()}.
select_sink(Channel, Severity, Application, Module) ->
    logi_sink_table:select(Channel, Severity, Application, Module).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Id]) ->
    _ = process_flag(trap_exit, true),
    State =
        #?STATE{
            id    = Id,
            table = logi_sink_table:new(Id)
           },
    {ok, State}.

%% @private
handle_call({install_sink,   Arg}, _, State) -> handle_install_sink(Arg, State);
handle_call({uninstall_sink, Arg}, _, State) -> handle_uninstall_sink(Arg, State);
handle_call({find_sink,      Arg}, _, State) -> handle_find_sink(Arg, State);
handle_call({set_condition,  Arg}, _, State) -> handle_set_condition(Arg, State);
handle_call(_,                     _, State) -> {noreply, State}.

%% @private
handle_cast(_, State) -> {noreply, State}.

%% @private
handle_info({'DOWN', Ref, _, _, _}, State) -> handle_down(Ref, State);
handle_info(_,                      State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_install_sink(Arg, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Arg     :: {logi_sink:sink(), timeout() | pid(), error | if_exists | supersede},
      Result  :: {ok, OldSink} | {error, Reason},
      OldSink :: undefined | logi_sink:sink(),
      Reason  :: {already_installed, logi_sink:sink()}.
handle_install_sink({Sink, Lifetime, IfExists}, State0) ->
    {OldSink, OldCancelLifetimeFun, Sinks0} = take_sink(logi_sink:get_id(Sink), State0#?STATE.sinks),
    case OldSink =:= undefined orelse IfExists =:= supersede of
        false ->
            case IfExists of
                error  -> {reply, {error, {already_installed, Sink}}, State0};
                ignore -> {reply, {ok, OldSink}, State0}
            end;
        true ->
            _  = OldCancelLifetimeFun(),
            ok = logi_sink_table:register(State0#?STATE.table, Sink, OldSink),
            {LifetimeRef, CancelLifetimeFun} = set_lifetime(Lifetime),
            Sinks1 = [{logi_sink:get_id(Sink), LifetimeRef, CancelLifetimeFun, Sink} | Sinks0],
            State1 = State0#?STATE{sinks = Sinks1},
            {reply, {ok, OldSink}, State1}
    end.

-spec handle_uninstall_sink(logi_sink:id(), #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: {ok, logi_sink:sink()} | error.
handle_uninstall_sink(SinkId, State0) ->
    case take_sink(SinkId, State0#?STATE.sinks) of
        {undefined, _, _}                -> {reply, error, State0};
        {Sink, CancelLifetimeFun, Sinks} ->
            _  = CancelLifetimeFun(),
            ok = logi_sink_table:deregister(State0#?STATE.table, Sink),
            State1 = State0#?STATE{sinks = Sinks},
            {reply, {ok, Sink}, State1}
    end.

-spec handle_find_sink(logi_sink:id(), #?STATE{}) -> {reply, {ok, logi_sink:sink()} | error, #?STATE{}}.
handle_find_sink(SinkId, State) ->
    case lists:keyfind(SinkId, 1, State#?STATE.sinks) of
        false           -> {reply, error, State};
        {_, _, _, Sink} -> {reply, {ok, Sink}, State}
    end.

-spec handle_set_condition({logi_sink:id(), logi_sink:condition()}, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: {ok, logi_sink:condition()} | error.
handle_set_condition({SinkId, Condition}, State0) ->
    case lists:keytake(SinkId, 1, State0#?STATE.sinks) of
        false                                        -> {reply, error, State0};
        {value, Entry = {_, _, _, Sink0}, Sinks} ->
            Sink1 = logi_sink:from_map(maps:put(condition, Condition, logi_sink:to_map(Sink0))),
            ok = logi_sink_table:register(State0#?STATE.table, Sink1, Sink0),
            State1 = State0#?STATE{sinks = [setelement(4, Entry, Sink1) | Sinks]},
            {reply, {ok, logi_sink:get_condition(Sink0)}, State1}
    end.

-spec handle_down(reference(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Ref, State0) ->
    case lists:keytake(Ref, 2, State0#?STATE.sinks) of
        false                           -> {noreply, State0};
        {value, {_, _, _, Sink}, Sinks} ->
            ok = logi_sink_table:deregister(State0#?STATE.table, Sink),
            State1 = State0#?STATE{sinks = Sinks},
            {noreply, State1}
    end.

-spec take_sink(logi_sink:id(), sinks()) -> {undefined | logi_sink:sink(), cancel_lifetime_fun(), sinks()}.
take_sink(SinkId, Sinks0) ->
    case lists:keytake(SinkId, 1, Sinks0) of
        false                                            -> {undefined, fun () -> ok end, Sinks0};
        {value, {_, _, CancelLifetimeFun, Sink}, Sinks1} -> {Sink, CancelLifetimeFun, Sinks1}
    end.

-spec set_lifetime(timeout() | pid()) -> {lifetime_ref(), cancel_lifetime_fun()}.
set_lifetime(infinity)             -> {undefined, fun () -> ok end};
set_lifetime(Pid) when is_pid(Pid) -> {monitor(process, Pid), fun erlang:demonitor/1};
set_lifetime(Time)                 ->
    TimeoutRef = make_ref(),
    TimerRef = erlang:send_after(Time, self(), {'DOWN', TimeoutRef, timeout, undefined, timeout}),
    {TimeoutRef, fun () -> erlang:cancel_timer(TimerRef, [{async, true}]) end}.

-spec is_valid_lifetime(timeout() | pid() | term()) -> boolean().
is_valid_lifetime(infinity)                                                               -> true;
is_valid_lifetime(Pid) when is_pid(Pid)                                                   -> true;
is_valid_lifetime(Timeout) when is_integer(Timeout), Timeout >= 0, Timeout < 16#100000000 -> true;
is_valid_lifetime(_)                                                                      -> false.
