%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
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
%% TODO: -export([install_sink_opt/3, install_sink_opt/4]).
-export([uninstall_sink/1, uninstall_sink/2]).
-export([set_sink_condition/2, set_sink_condition/3]).
-export([find_sink/1,  find_sink/2]).
-export([whereis_sink_proc/1, whereis_sink_proc/2]).
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
          id        :: logi_sink:id(),
          condition :: logi_condition:condition(),
          sink      :: logi_sink:sink(),
          child_id  :: logi_sink_proc:child_id(),
          writer    :: logi_sink_writer:writer() | undefined,
          monitor   :: reference()
        }).

-type sinks() :: [#sink{}].

-type id() :: atom().
%% The identifier of a channel

-type install_sink_options() :: [install_sink_option()].

-type install_sink_option() ::  {channel, id()}
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

-type installed_sink() ::
        #{
           condition => logi_condition:condition(),
           sink      => logi_sink:sink(),
           child_id  => logi_sink_proc:child_id(),
           writer    => logi_sink_writer:writer() | undefined
         }.
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
    case logi_channel_set_sup:start_child(Channel) of
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
delete(Channel) when is_atom(Channel) -> logi_channel_set_sup:stop_child(Channel);
delete(Channel)                       -> error(badarg, [Channel]).

%% @doc Returns a list of all existing channels
-spec which_channels() -> [id()].
which_channels() -> logi_channel_set_sup:which_children().

%% TODO: install_sink_opt/3を用意しても良いかもしれない (install_sink/2系は第一引数にチャンネルを取るようにする)

%% %% @equiv install_sink(Condition, Sink, [])
-spec install_sink(logi_sink:sink(), logi_condition:condition()) -> install_sink_result(). % TODO: install_sink_result/0は型にしなくても良い
install_sink(Sink, Condition) -> install_sink(Sink, Condition, []).

%% @doc Installs `Sink'
%%
%% TODO: notice: This function may block if instantiate/1 of the Sink blocks
-spec install_sink(logi_sink:sink(), logi_condition:condition(), install_sink_options()) -> install_sink_result().
install_sink(Sink, Condition, Options) ->
    Args = [Sink, Condition, Options],
    _ = logi_condition:is_condition(Condition) orelse error(badarg, Args),
    _ = logi_sink:is_sink(Sink) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Channel  = proplists:get_value(channel, Options, default_channel()),
    IfExists = proplists:get_value(if_exists, Options, error),
    _ = lists:member(IfExists, [error, ignore, supersede]) orelse error(badarg, Args),

    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, Args),
    gen_server:call(Pid, {install_sink, {Sink, Condition, IfExists}}).

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

%% @equiv set_sink_condition(SinkId, Condition, [])
-spec set_sink_condition(logi_sink:id(), logi_condition:condition()) -> {ok, Old::logi_condition:condition()} | error.
set_sink_condition(SinkId, Condition) ->
    set_sink_condition(SinkId, Condition, []).

%% @doc TODO
-spec set_sink_condition(logi_sink:id(), logi_condition:condition(), Options) -> {ok, Old} | error when
      Options :: [Option],
      Option  :: {channel, id()},
      Old     :: logi_condition:condition().
set_sink_condition(SinkId, Condition, Options) ->
    Args = [SinkId, Condition, Options],
    _ = is_atom(SinkId) orelse error(badarg, Args),
    _ = logi_condition:is_condition(Condition) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Channel = proplists:get_value(channel, Options, default_channel()),
    Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Channel, Args),
    gen_server:call(Pid, {set_sink_condition, {SinkId, Condition}}).

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

%% @equiv whereis_sink_proc(Path, [])
-spec whereis_sink_proc([logi_sink:id()]) -> pid() | undefined.
whereis_sink_proc(Path) ->
    whereis_sink_proc(Path, []).

%% TODO: doc
-spec whereis_sink_proc([logi_sink:id()], Options) -> pid() | undefined when
      Options :: [{channel, id()}].
whereis_sink_proc([], Options) ->
    error(badarg, [[], Options]);
whereis_sink_proc([SinkId | Path], Options) ->
    case find_sink(SinkId, Options) of
        error      -> undefined;
        {ok, Sink} ->
            (fun Loop (Sup, []) ->
                     logi_sink_sup:get_child_sink(Sup);
                 Loop (Sup, [NextId | Rest]) ->
                     case logi_sink_sup:find_grandchild(Sup, NextId) of
                         error          -> undefined;
                         {ok, ChildSup} -> Loop(ChildSup, Rest)
                     end
             end)(maps:get(child_id, Sink), Path)
    end.

%% @equiv which_sinks([])
-spec which_sinks() -> [logi_sink:id()].
which_sinks() -> which_sinks([]).

%% @doc Returns a list of installed sinks
%%
%% The default value of the `channel' option is `logi_channel:default_channel()'.
%%
%% TODO: doc: result list includes restarting sinks
-spec which_sinks(Options) -> [logi_sink:id()] when
      Options :: [{channel, Channel}],
      Channel :: id().
which_sinks(Options) ->
    _ = is_list(Options) orelse error(badarg, [Options]),

    Channel = proplists:get_value(channel, Options, default_channel()),
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
    State =
        #?STATE{
            id    = Id,
            table = logi_sink_table:new(Id)
           },
    {ok, State}.

%% @private
handle_call({install_sink,   Arg},     _, State) -> handle_install_sink(Arg, State);
handle_call({uninstall_sink, Arg},     _, State) -> handle_uninstall_sink(Arg, State);
handle_call({set_sink_condition, Arg}, _, State) -> handle_set_sink_condition(Arg, State);
handle_call({find_sink,      Arg},     _, State) -> handle_find_sink(Arg, State);
handle_call(_,                         _, State) -> {noreply, State}.

%% @private
handle_cast(_, State) -> {noreply, State}.

%% @private
handle_info({sink_writer, ChildId, Writer}, State) -> handle_sink_writer(ChildId, Writer, State);
handle_info({'DOWN', Ref, _, _, _}, State)         -> handle_down(Ref, State);
handle_info(_,                      State)         -> {noreply, State}.

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
      Arg      :: {logi_sink:sink(), logi_condition:condition(), IfExists},
      IfExists :: error | if_exists | supersede,
      Result   :: {ok, OldSink} | {error, Reason},
      OldSink  :: undefined | installed_sink(),
      Reason   :: {already_installed, installed_sink()}.
handle_install_sink({Sink, Condition, IfExists}, State0) ->
    SinkId = logi_sink:get_id(Sink),
    {OldSink, OldCondition, Sinks} =
        case lists:keytake(SinkId, #sink.id, State0#?STATE.sinks) of
            false                     -> {undefined, [], State0#?STATE.sinks};
            {value, OldSink0, Sinks0} -> {OldSink0, OldSink0#sink.condition, Sinks0}
        end,
    case OldSink =:= undefined orelse IfExists =:= supersede of
        false ->
            case IfExists of
                error  -> {reply, {error, {already_installed, to_installed_sink(OldSink)}}, State0};
                ignore -> {reply, {ok, to_installed_sink(OldSink)}, State0}
            end;
        true ->
            case logi_sink_proc:start_root_child(Sink) of
                {error, Reason} -> {reply, {error, Reason}, State0};
                {ok, ChildId}   ->
                    Writer = logi_sink_proc:recv_writer_from_child(ChildId, 1000),
                    _ = case Writer =:= undefined of
                            true  -> logi_sink_table:deregister(State0#?STATE.table, SinkId, OldCondition);
                            false -> logi_sink_table:register(State0#?STATE.table, SinkId, Writer, Condition, OldCondition)
                        end,
                    ok = release_sink_instance(OldSink, State0),
                    Entry =
                        #sink{
                           id        = SinkId,
                           condition = Condition,
                           sink      = Sink,
                           writer    = Writer,
                           child_id  = ChildId,
                           monitor   = monitor(process, ChildId)
                          },
                    State1 = State0#?STATE{sinks = [Entry | Sinks]},
                    {reply, {ok, to_installed_sink(OldSink)}, State1}
            end
    end.

-spec handle_set_sink_condition(Arg, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Arg    :: {logi_sink:id(), New::logi_condition:condition()},
      Result :: {ok, Old::logi_condition:condition()} | error.
handle_set_sink_condition({SinkId, Condition}, State0) ->
    case lists:keytake(SinkId, #sink.id, State0#?STATE.sinks) of
        false                 -> {reply, error, State0};
        {value, Sink0, Sinks} ->
            ok = logi_sink_table:register(State0#?STATE.table, SinkId, Sink0#sink.writer, Condition, Sink0#sink.condition),
            Sink1 = Sink0#sink{condition = Condition},
            State1 = State0#?STATE{sinks = [Sink1 | Sinks]},
            {reply, {ok, Sink0#sink.condition}, State1}
    end.

-spec handle_uninstall_sink(logi_sink:id(), #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: {ok, installed_sink()} | error.
handle_uninstall_sink(SinkId, State0) ->
    case lists:keytake(SinkId, #sink.id, State0#?STATE.sinks) of
        false                -> {reply, error, State0};
        {value, Sink, Sinks} ->
            ok = logi_sink_table:deregister(State0#?STATE.table, SinkId, Sink#sink.condition),
            ok = release_sink_instance(Sink, State0),
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
    case lists:keytake(Ref, #sink.monitor, State0#?STATE.sinks) of
        false                 -> {noreply, State0};
        {value, Sink0, Sinks} ->
            ok = logi_sink_table:deregister(State0#?STATE.table, Sink0#sink.id, Sink0#sink.condition),
            ok = release_sink_instance(Sink0, State0),
            {noreply, State0#?STATE{sinks = Sinks}}
    end.

-spec handle_sink_writer(logi_sink_proc:child_id(), logi_sink_writer:writer()|undefined, #?STATE{}) -> {noreply, #?STATE{}}.
handle_sink_writer(ChildId, Writer, State = #?STATE{table = Table}) ->
    case lists:keytake(ChildId, #sink.child_id, State#?STATE.sinks) of
        false                 -> {noreply, State};
        {value, Sink0, Sinks} ->
            _ = case Writer =:= undefined of
                    true  -> logi_sink_table:deregister(Table, Sink0#sink.id, Sink0#sink.condition);
                    false -> logi_sink_table:register(Table, Sink0#sink.id, Writer, Sink0#sink.condition, Sink0#sink.condition)
                end,
            Sink1 = Sink0#sink{writer = Writer},
            {noreply, State#?STATE{sinks = [Sink1 | Sinks]}}
    end.

-spec to_installed_sink(#sink{} | undefined) -> installed_sink().
to_installed_sink(undefined) ->
    undefined;
to_installed_sink(Sink) ->
    #{
       condition => Sink#sink.condition,
       sink      => Sink#sink.sink,
       writer    => Sink#sink.writer,
       child_id  => Sink#sink.child_id
     }.

-spec release_sink_instance(undefined | #sink{}, #?STATE{}) -> ok.
release_sink_instance(undefined, _State) ->
    ok;
release_sink_instance(Sink, _State) ->
    _ = demonitor(Sink#sink.monitor, [flush]),
    logi_sink_proc:stop_child(Sink#sink.child_id).
