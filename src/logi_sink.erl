%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sinks
%%
%% A sink will consume the log messages sent to the channel which the sink have been installed.
%%
%% The main purpose of sinks is to write messages to some output devices (e.g. tty, file, socket).
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% > ok = logi_channel:create(sample_log).
%% > WriteFun = fun (_, _, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
%% > Sink = logi_builtin_sink_fun:new(WriteFun).
%% > {ok, _} = logi_channel:install_sink(info, Sink, [{id, my_sink}, {channel, sample_log}]).
%% > logi:info("Hello World", [], [{logger, sample_log}]).
%% [my_sink] Hello World  % 'logi_builtin_sink_fun:write/4' was invoked
%% </pre>
%%
%% Sinks have an associated layout:
%% <pre lang="erlang">
%% > WriteFun = fun (Context, Layout, Format, Data) -> io:format(logi_layout:format(Context, Format, Data, Layout)) end.
%% > Sink = logi_builtin_sink_fun:new(WriteFun).
%% > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[EXAMPLE] " ++ Format ++"\n", Data) end).
%% > {ok, _} = logi_channel:install_sink(info, Sink, [{layout, Layout}]). % Installs `Sink' to the default channel
%% > logi:info("hello world").
%% [EXAMPLE]hello world
%%
%% %% If 'layout' option is not specified, the result of `logi_sink:default_layout(Sink)' will be used instead.
%% > {ok, _} = logi_channel:install_sink(info, Sink, [{if_exists, supersede}]).
%% > logi:info("hello world").
%% 2015-11-09 22:18:33.934 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
%%
%% A channel can have multiple sinks:
%% <pre lang="erlang">
%% > ok = logi_channel:create(sample_log).
%% > WriteFun_0 = fun (_, _, Format, Data) -> io:format("[sink_0] " ++ Format ++ "\n", Data) end.
%% > WriteFun_1 = fun (_, _, Format, Data) -> io:format("[sink_1] " ++ Format ++ "\n", Data) end.
%% > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun_0), [{id, sink_0}, {channel, sample_log}]).
%% > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun_1), [{id, sink_1}, {channel, sample_log}]).
%% > logi:info("Hello World", [], [{logger, sample_log}]).
%% [sink_0] Hello World
%% [sink_1] Hello World
%% </pre>
-module(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2]).
-export([is_sink/1]).
-export([is_callback_module/1]).
-export([get_module/1]).
-export([get_arg/1]).
-export([make_noop_process_spec/2]).

%% TODO: 別モジュールにする
-export([notify_started/2]).
-export([notify_stopped/1]).
-export([recv_started/1]).
-export([start_child/2]).
-export([stop_child/2]).
-export([make_root_parent/1]).

-export_type([sink/0]).
-export_type([callback_module/0]).
-export_type([arg/0]).
-export_type([parent/0]).
-export_type([sink_sup/0]).
-export_type([id/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
%% make_writer_spec
-callback make_process_spec(logi_sink:parent(), arg()) -> supervisor:child_spec().

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(SINK, ?MODULE).
-record(?SINK,
        {
          module    :: callback_module(),
          arg       :: arg(),
          sup_flags :: supervisor:sup_flags()
        }).

-opaque sink() :: #?SINK{}.
-type callback_module() :: module().
-type arg() :: term().
-type parent() :: {pid(), sink_sup()}.
-type sink_sup() :: pid().
-type id() :: atom().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(callback_module(), arg()) -> sink().
new(Module, Arg) ->
    _ = is_callback_module(Module) orelse error(badarg, [Module, Arg]),
    #?SINK{
        module = Module,
        arg = Arg,
        sup_flags = #{strategy => one_for_all} % TODO:
       }.

%% @doc Returns `true' if `X' is a sink instance, otherwise `false'
-spec is_sink(X :: (sink() | term())) -> boolean().
is_sink(#?SINK{module = Module}) -> is_callback_module(Module);
is_sink(_)                       -> false.

%% @doc Returns `true' if `X' is a module which implements the `sink' behaviour, otherwise `false'
-spec is_callback_module(X :: (callback_module() | term())) -> boolean().
is_callback_module(X) -> (is_atom(X) andalso logi_utils:function_exported(X, make_process_spec, 2)).

-spec get_module(sink()) -> callback_module().
get_module(#?SINK{module = Module}) -> Module.

-spec get_arg(sink()) -> arg().
get_arg(#?SINK{arg = Arg}) -> Arg.

%% make_stateless_writer_spec (or standalone)
-spec make_noop_process_spec(parent(), logi_sink_writer:writer()) -> supervisor:child_spec().
make_noop_process_spec(Parent, Writer) ->
    #{
       id => logi_sink_writer:get_module(Writer),
       start => {logi_sink_noop, start_link, [Parent, Writer]},
       restart => temporary
     }.

-spec make_root_parent(sink_sup()) -> parent().
make_root_parent(SinkSup) ->
    {self(), SinkSup}.

-spec notify_started(parent(), logi_sink_writer:writer()) -> ok.
notify_started({Pid, Sup}, Writer) ->
    _ = Pid ! {'LOGI_SINK_STARTED', Sup, self(), Writer},
    ok.

-spec notify_stopped(parent()) -> ok.
notify_stopped({Pid, Sup}) ->
    _ = Pid ! {'LOGI_SINK_STOPPED', Sup, self()},
    ok.

-spec recv_started(sink_sup()) -> logi_sink_writer:writer().
recv_started(SinkSup) ->
    receive
        {'LOGI_SINK_STARTED', SinkSup, _, Writer} -> Writer
    end.

-spec start_child(parent(), sink()) -> {ok, sink_sup()} | {error, Reason::term()}.
start_child({_, ParentSup}, Sink) ->
    ChildrenSup = logi_sink_set_sup:get_current_process(ParentSup), % TODO
    case logi_sink_set_sup:start_sink_sup(ChildrenSup, Sink#?SINK.sup_flags) of
        {error, Reason} -> {error, Reason};
        {ok, SinkSup}   ->
            Parent = {self(), SinkSup},
            try
                ChildSpec = (Sink#?SINK.module):make_process_spec(Parent, Sink#?SINK.arg),
                case logi_sink_sup:start_sink(SinkSup, ChildSpec) of
                    {error, Reason} ->
                        ok = logi_sink_set_sup:stop_sink_sup(ChildrenSup, SinkSup),
                        {error, Reason};
                    {ok, _SinkPid} ->
                        {ok, SinkSup}
                end
            catch
                ExClass:ExReason ->
                    ok = logi_sink_set_sup:stop_sink_sup(ChildrenSup, SinkSup),
                    erlang:raise(ExClass, ExReason, erlang:get_stacktrace())
            end
    end.

-spec stop_child(parent(), sink_sup()) -> ok.
stop_child({_, ParentSup}, SinkSup) ->
    ChildrenSup = logi_sink_set_sup:get_current_process(ParentSup), % TODO
    logi_sink_set_sup:stop_sink_sup(ChildrenSup, SinkSup).
