%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Message Channels
%%
%% A channel (logically) receives log messages from loggers
%% and delivers the messages to installed sinks.
%%
%% == EXAMPLE ==
%% Basic usage:
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% %%
%% %% CREATE CHANNEL
%% %%
%% > ok = logi_channel:create(sample_log).
%% > logi_channel:which_channels().
%% [sample_log,logi_default_log]  % 'logi_default_log' is created automatically when 'logi' application was started
%%
%% %%
%% %% INSTALL SINK
%% %%
%% > WriteFun = fun (_, _, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
%% > Sink = logi_builtin_sink_fun:new(WriteFun).
%% > {ok, _} = logi_channel:install_sink(info, Sink, [{channel, sample_log}]). % Installs `Sink' with `info' level
%% > logi_channel:which_sinks([{channel, sample_log}]).
%% [logi_builtin_sink_fun]
%%
%% %%
%% %% OUTPUT LOG MESSAGE
%% %%
%% > logi:debug("hello world", [], [{logger, sample_log}]).
%% % The message is not emitted (the severity is too low).
%%
%% > logi:info("hello world", [], [{logger, sample_log}]).
%% [my_sink] hello world
%%
%% > logi:alert("hello world", [], [{logger, sample_log}]).
%% [my_sink] hello world
%%
%% > logi:info("hello world"). % If `logger' option is omitted, the default channel will be used
%% % The message is not emitted (no sinks are installed to the default channel).
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
-export([uninstall_sink/1, uninstall_sink/2]).
-export([update_sink/2]).
-export([find_sink/1,  find_sink/2]).
-export([which_sinks/0, which_sinks/1]).

-export_type([id/0]).
-export_type([install_sink_option/0, install_sink_options/0]).
-export_type([install_sink_result/0]).
-export_type([uninstall_sink_result/0]).
-export_type([installed_sink/0]).

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

-record(sink,
        {
          id                  :: logi_sink:id(),
          condition           :: logi_sink:condition(),
          instance            :: logi_sink:sink(),
          lifetime_ref        :: lifetime_ref(),
          cancel_lifetime_fun :: cancel_lifetime_fun()
        }).

-type sinks() :: [#sink{}].

-type lifetime_ref() :: undefined | reference().
-type cancel_lifetime_fun() :: fun (() -> any()).

-type id() :: atom().
%% The identifier of a channel

-type install_sink_options() :: [install_sink_option()].

-type install_sink_option() :: {id, logi_sink:id()}
                             | {channel, id()}
                             | {lifetime, timeout() | pid()}
                             | {if_exists, error | ignore | supersede}.
%% Let `Sink' be the sink which is subject of the installation.
%%
%% `id':
%% - The identifier of `Sink'
%% - default: `logi_sink:get_module(Sink)'
%%
%% `channel':
%% - The channel in which `Sink' will be installed
%% - default: `logi_channel:default_channel()'
%%
%% `lifetime':
%% - The lifetime of `Sink'.
%% - When `timeout()' expires or `pid()' exits, the sink will be automatically uninstalled from the channel.
%% - default: `infinity'
%%
%% `if_exists':
%% - The confliction handling policy.
%% - If a sink with the same identifier already exists,
%% &#x20;&#x20;- `error': the function returns an error `{error, {already_installed, ExistingSink}}'.
%% &#x20;&#x20;- `ignore': the new sink is ignored. Then the function returns `{ok, ExistingSink}'.
%% &#x20;&#x20;- `supersede': the new sink supersedes it. Then the function returns `{ok, OldSink}'.
%% - default: `error'

-type install_sink_result() :: {ok, OldSink :: undefined | installed_sink()}
                             | {error, {already_installed, installed_sink()}}.
%% The result of {@link install_sink/2}.
%%
%% If there does not exist a sink which has the same identifier with a new one,
%% the function returns `{ok, undefined}'.
%%
%% Otherwise the result value depends on the value of the `if_exists' option
%% (see the description of `install_sink_option/0' for details).

-type uninstall_sink_result() :: {ok, installed_sink()} | error.
%% The result of {@link uninstall_sink/2}.
%%
%% The function returns `{ok, Sink}' if the specified sink exists in the channel, `error' otherwise.

-type installed_sink() :: {logi_sink:condition(), logi_sink:sink()}.
%% The information of an installed sink

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc The default channel
%%
%% This channel is created automatically when `logi' application was started.
%%
%% NOTE: The default channel ID is the same as the default logger ID ({@link logi:default_logger/0})
-spec default_channel() -> id().
default_channel() -> logi:default_logger().

%% @doc Creates a new channel
%%
%% If the channel already exists, nothing happens.
%%
%% If there exists a process or a ETS table with the same name as `Channel', the function crashes.
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
%% If the channel does not exists, it is silently ignored.
-spec delete(id()) -> ok.
delete(Channel) when is_atom(Channel) -> logi_channel_sup:stop_child(Channel);
delete(Channel)                       -> error(badarg, [Channel]).

%% @doc Returns a list of all existing channels
-spec which_channels() -> [id()].
which_channels() -> logi_channel_sup:which_children().

%% @equiv install_sink(Condition, Sink, [])
-spec install_sink(logi_sink:condition(), logi_sink:sink()) -> install_sink_result().
install_sink(Condition, Sink) -> install_sink(Condition, Sink, []).

%% @doc Installs `Sink'
-spec install_sink(logi_sink:condition(), logi_sink:sink(), install_sink_options()) -> install_sink_result().
install_sink(Condition, Sink, Options) ->
    Args = [Condition, Sink, Options],
    _ = logi_sink:is_condition(Condition) orelse error(badarg, Args),
    _ = logi_sink:is_sink(Sink) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Id       = proplists:get_value(id, Options, logi_sink:get_module(Sink)),
    Channel  = proplists:get_value(channel, Options, default_channel()),
    IfExists = proplists:get_value(if_exists, Options, error),
    Lifetime = proplists:get_value(lifetime, Options, infinity),
    _ = is_atom(Id) orelse error(badarg, Args),
    _ = lists:member(IfExists, [error, ignore, supersede]) orelse error(badarg, Args),
    _ = is_valid_lifetime(Lifetime) orelse error(badarg, Args),

    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, Args),
    gen_server:call(Pid, {install_sink, {Id, Sink, Condition, Lifetime, IfExists}}).

%% @equiv uninstall_sink(SinkId, [])
-spec uninstall_sink(logi_sink:id()) -> uninstall_sink_result().
uninstall_sink(SinkId) -> uninstall_sink(SinkId, []).

%% @doc Uninstalls the sink which has the identifier `SinkId' from `Channel'
%%
%% The default value of the `channel' option is `logi_channel:default_channel()'.
-spec uninstall_sink(logi_sink:id(), Options) -> uninstall_sink_result() when
      Options :: [{channel, Channel}],
      Channel :: id().
uninstall_sink(SinkId, Options) ->
    _ = is_atom(SinkId) orelse error(badarg, [SinkId, Options]),
    _ = is_list(Options) orelse error(badarg, [SinkId, Options]),

    Channel = proplists:get_value(channel, Options, default_channel()),
    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, [SinkId, Options]),
    gen_server:call(Pid, {uninstall_sink, SinkId}).

%% @doc Updates the sink which associated to the identifier `SinkId'
%%
%% This function returns `{ok, Old}' if such a sink exists, `error' otherwise.
%% `Old' is the old contents of the sink.
%%
%% === OPTIONS ===
%% `channel':
%% - The channel in which `SinkId' is installed
%% - default: `logi_channel:default_channel()'
%%
%% `condition':
%% - A new condition
%% - default: The current condition associated to `SinkId'
%%
%% `sink':
%% - A new sink instance
%% - default: The current sink instance associated to `SinkId'
-spec update_sink(logi_sink:id(), Options) -> {ok, Old} | error when
      Options :: [Option],
      Option  :: {channel, id()}
               | {condition, logi_sink:condition()}
               | {sink, logi_sink:sink()},
      Old :: installed_sink().
update_sink(SinkId, Options) ->
    Args = [SinkId, Options],
    _ = is_atom(SinkId) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Channel = proplists:get_value(channel, Options, default_channel()),
    Condition = proplists:get_value(condition, Options),
    Sink = proplists:get_value(sink, Options),
    _ = Condition =:= undefined orelse logi_sink:is_condition(Condition) orelse error(badarg, Args),
    _ = Sink =:= undefined orelse logi_sink:is_sink(Sink) orelse error(badarg, Args),

    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, Args),
    gen_server:call(Pid, {update_sink, {SinkId, Sink, Condition}}).

%% @equiv find_sink(SinkId, [])
-spec find_sink(logi_sink:id()) -> {ok, Sink :: installed_sink()} | error.
find_sink(SinkId) -> find_sink(SinkId, []).

%% @doc Searchs for `SinkId' in `Channel'; returns `{ok, Sink}', or `error' if `SinkId' is not present
%%
%% The default value of the `channel' option is `logi_channel:default_channel()'.
-spec find_sink(logi_sink:id(), Options) -> {ok, Sink} | error when
      Options :: [{channel, Channel}],
      Channel :: id(),
      Sink    :: installed_sink().
find_sink(SinkId, Options) ->
    _ = is_atom(SinkId) orelse error(badarg, [SinkId, Options]),
    _ = is_list(Options) orelse error(badarg, [SinkId, Options]),

    Channel = proplists:get_value(channel, Options, default_channel()),
    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, [SinkId, Options]),
    gen_server:call(Pid, {find_sink, SinkId}).

%% @equiv which_sinks([])
-spec which_sinks() -> [logi_sink:id()].
which_sinks() -> which_sinks([]).

%% @doc Returns a list of installed sinks
%%
%% The default value of the `channel' option is `logi_channel:default_channel()'.
-spec which_sinks(Options) -> [logi_sink:id()] when
      Options :: [{channel, Channel}],
      Channel :: id().
which_sinks(Options) ->
    _ = is_list(Options) orelse error(badarg, [Options]),

    Channel = proplists:get_value(channel, Options, default_channel()),
    _ = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, [Options]),
    logi_sink_table:which_sinks(Channel).

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
%% If the channel does not exist, it returns an empty list.
%%
%% @private
-spec select_sink(id(), logi:severity(), atom(), module()) -> logi_sink_table:select_result().
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
handle_call({update_sink,  Arg},   _, State) -> handle_update_sink(Arg, State);
handle_call({find_sink,      Arg}, _, State) -> handle_find_sink(Arg, State);
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
      Arg      :: {logi_sink:id(), logi_sink:sink(), logi_sink:condition(), timeout() | pid(), IfExists},
      IfExists :: error | if_exists | supersede,
      Result   :: {ok, OldSink} | {error, Reason},
      OldSink  :: undefined | logi_sink:sink(),
      Reason   :: {already_installed, logi_sink:sink()}.
handle_install_sink({SinkId, Sink, Condition, Lifetime, IfExists}, State0) ->
    {OldSink, OldCondition, OldCancelLifetimeFun, Sinks} =
        case lists:keytake(SinkId, #sink.id, State0#?STATE.sinks) of
            false                     -> {undefined, [], fun () -> ok end, State0#?STATE.sinks};
            {value, OldSink0, Sinks0} -> {OldSink0, OldSink0#sink.condition, OldSink0#sink.cancel_lifetime_fun, Sinks0}
        end,
    case OldSink =:= undefined orelse IfExists =:= supersede of
        false ->
            case IfExists of
                error  -> {reply, {error, {already_installed, to_installed_sink(OldSink)}}, State0};
                ignore -> {reply, {ok, to_installed_sink(OldSink)}, State0}
            end;
        true ->
            _  = OldCancelLifetimeFun(),
            ok = logi_sink_table:register(State0#?STATE.table, SinkId, Sink, Condition, OldCondition),
            {LifetimeRef, CancelLifetimeFun} = set_lifetime(Lifetime),
            Entry =
                #sink{
                   id                  = SinkId,
                   condition           = Condition,
                   instance            = Sink,
                   lifetime_ref        = LifetimeRef,
                   cancel_lifetime_fun = CancelLifetimeFun
                  },
            State1 = State0#?STATE{sinks = [Entry | Sinks]},
            {reply, {ok, to_installed_sink(OldSink)}, State1}
    end.

-spec handle_update_sink(Arg, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Arg    :: {logi_sink:id(), logi_sink:sink(), logi_sink:condition()},
      Result :: {ok, installed_sink()} | error.
handle_update_sink({SinkId, MaybeSinkInstance, MaybeCondition}, State0) ->
    case lists:keytake(SinkId, #sink.id, State0#?STATE.sinks) of
        false                 -> {reply, error, State0};
        {value, Sink0, Sinks} ->
            Condition =
                case MaybeCondition of
                    undefined -> Sink0#sink.condition;
                    _         -> MaybeCondition
                end,
            SinkInstance =
                case MaybeSinkInstance of
                    undefined -> Sink0#sink.instance;
                    _         -> MaybeSinkInstance
                end,
            ok = logi_sink_table:register(State0#?STATE.table, SinkId, SinkInstance, Condition, Sink0#sink.condition),
            Sink1 = Sink0#sink{instance = SinkInstance, condition = Condition},
            State1 = State0#?STATE{sinks = [Sink1 | Sinks]},
            {reply, {ok, to_installed_sink(Sink0)}, State1}
    end.

-spec handle_uninstall_sink(logi_sink:id(), #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: {ok, installed_sink()} | error.
handle_uninstall_sink(SinkId, State0) ->
    case lists:keytake(SinkId, #sink.id, State0#?STATE.sinks) of
        false                -> {reply, error, State0};
        {value, Sink, Sinks} ->
            _  = (Sink#sink.cancel_lifetime_fun)(),
            ok = logi_sink_table:deregister(State0#?STATE.table, SinkId, Sink#sink.condition),
            State1 = State0#?STATE{sinks = Sinks},
            {reply, {ok, to_installed_sink(Sink)}, State1}
    end.

-spec handle_find_sink(logi_sink:id(), #?STATE{}) -> {reply, {ok, installed_sink()} | error, #?STATE{}}.
handle_find_sink(SinkId, State) ->
    case lists:keyfind(SinkId, #sink.id, State#?STATE.sinks) of
        false -> {reply, error, State};
        Sink  -> {reply, {ok, to_installed_sink(Sink)}, State}
    end.

-spec handle_down(reference(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Ref, State0) ->
    case lists:keytake(Ref, #sink.lifetime_ref, State0#?STATE.sinks) of
        false                -> {noreply, State0};
        {value, Sink, Sinks} ->
            ok = logi_sink_table:deregister(State0#?STATE.table, Sink#sink.id, Sink#sink.condition),
            State1 = State0#?STATE{sinks = Sinks},
            {noreply, State1}
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

-spec to_installed_sink(#sink{} | undefined) -> installed_sink().
to_installed_sink(undefined) ->
    undefined;
to_installed_sink(#sink{condition = Condition, instance = Instance}) ->
    {Condition, Instance}.
