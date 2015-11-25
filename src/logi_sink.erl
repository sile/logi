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
-export([is_spec/1]).
-export([is_callback_module/1]).
-export([get_module/1, get_extra_data/1]).

-export([instantiate/2]).
-export([cleanup/2]).

-export([write/4]).

-export_type([id/0]).
-export_type([sink/0]).
-export_type([callback_module/0]).
-export_type([extra_data/0]).
-export_type([spec/0]).
-export_type([written_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_context:context(), io:format(), logi_layout:data(), extra_data()) -> written_data().


%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque sink() :: {callback_module(), extra_data()}.
%% A sink instance.

-type id() :: atom().
%% The identifier of a sink.
%% The sinks installed in the same channel must have different identifiers.

-type callback_module() :: module().
%% A module that implements the `logi_sink' behaviour.

-type extra_data() :: term().
%% The value of the fourth arguemnt of the `write/4' callback function.
%%
%% NOTE:
%% This value will be loaded from ETS every time the `write/4' is called.
%% Therefore, very huge data can cause a performance issue.

-type spec() :: sink() | logi_sink_agent:spec().

-type written_data() :: logi_layout:formatted_data().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new sink instance
-spec new(callback_module(), extra_data()) -> sink().
new(Module, ExtraData) ->
    _ = is_callback_module(Module) orelse error(badarg, [Module, ExtraData]),
    {Module, ExtraData}.

%% @doc Returns `true' if `X' is a sink instance, otherwise `false'
-spec is_sink(X :: (sink() | term())) -> boolean().
is_sink({Module, _}) -> is_callback_module(Module);
is_sink(_)           -> false.

%% @doc Returns `true' if `X' is a `sink()' object, otherwise `false'
-spec is_spec(X :: (spec() | term())) -> boolean().
is_spec(X) -> is_sink(X) orelse logi_sink_agent:is_spec(X).

%% @doc Gets the module of `Sink'
-spec get_module(Sink :: sink()) -> callback_module().
get_module({Module, _}) -> Module.

%% @doc Gets the extra data of `Sink'
-spec get_extra_data(Sink :: sink()) -> extra_data().
get_extra_data({_, ExtraData}) -> ExtraData.

%% @doc Returns `true' if `X' is a module which implements the `sink' behaviour, otherwise `false'
-spec is_callback_module(X :: (callback_module() | term())) -> boolean().
is_callback_module(X) -> (is_atom(X) andalso logi_utils:function_exported(X, write, 4)).

%% @doc Writes a log message
%%
%% If it fails to write, an exception will be raised.
-spec write(logi_context:context(), io:format(), logi_layout:data(), sink()) -> written_data().
write(Context, Format, Data, {Module, ExtraData}) ->
    Module:write(Context, Format, Data, ExtraData).

-spec instantiate(ParentSup, spec()) -> {ok, sink(), AgentSup, AgentPid} | {error, Reason} when
      ParentSup :: logi_sink_agent:agent_set_sup(),
      AgentSup  :: logi_sink_agent:agent_sup() | undefined,
      AgentPid  :: logi_sink_agent:agent() | undefined,
      Reason    :: term().
instantiate(ParentSup, Spec) ->
    _ = is_pid(ParentSup) orelse error(badarg, [ParentSup, Spec]),
    _ = logi_sink:is_spec(Spec) orelse error(badarg, [ParentSup, Spec]),
    case logi_sink_agent:is_spec(Spec) of
        false -> {ok, Spec, undefined, undefined};
        true  -> logi_sink_agent:start_agent(ParentSup, Spec)
    end.

%% TODO: change nameXXX: name
-spec cleanup(ParentSup, AgentSup) -> ok when
      ParentSup :: logi_sink_agent:agent_set_sup(),
      AgentSup  :: logi_sink_agent:agent_sup() | undefined.
cleanup(_ParentSup, undefined) ->
    ok;
cleanup(ParentSup, AgentSup) ->
    logi_sink_agent_set_sup:stop_agent_sup(ParentSup, AgentSup).
