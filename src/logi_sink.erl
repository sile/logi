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
-export([new/1, new/2]).
-export([from_writer/2]).
-export([is_sink/1]).

-export([get_id/1]).
-export([get_spec/1]).
-export([get_sup_flags/1]).

-export_type([sink/0]).
-export_type([id/0]).
-export_type([spec/0]).
-export_type([sup_flags/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(SINK, ?MODULE).
-record(?SINK,
        {
          spec      :: spec(),
          sup_flags :: sup_flags()
        }).

-type sink() :: #?SINK{}.
-type id() :: term().
-type spec() :: supervisor:childspec().
%% TODO: note: restart is ignored
-type sup_flags() :: sup_flags:sup_flags().
%% TODO: note: strategy is ignored

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(spec()) -> sink().
new(Spec) ->
    new(Spec, #{}).

-spec new(spec(), sup_flags()) -> sink().
new(Spec, Flags) ->
    %% TODO: validate
    #?SINK{
        spec      = normalize_spec(Spec),
        sup_flags = normalize_sup_flags(Flags)
       }.

-spec from_writer(id(), logi_sink_writer:writer()) -> sink().
from_writer(Id, Writer) ->
    new(#{id => Id, start => {logi_sink_noop, start_link, [Writer]}}).

-spec is_sink(sink() | term()) -> boolean().
is_sink(X) ->
    is_record(X, ?SINK).

-spec get_id(sink()) -> id().
get_id(#?SINK{spec = Spec}) ->
    maps:get(id, Spec).

-spec get_spec(sink()) -> spec().
get_spec(#?SINK{spec = Spec}) ->
    Spec.

-spec get_sup_flags(sink()) -> sup_flags().
get_sup_flags(#?SINK{sup_flags = Flags}) ->
    Flags.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec normalize_sup_flags(sup_flags()) -> sup_flags().
normalize_sup_flags({_, Intensity, Period}) ->
    #{intensity => Intensity, period => Period};
normalize_sup_flags(Flags) ->
    maps:remove(strategy, Flags).

-spec normalize_spec(spec()) -> spec().
normalize_spec({Id, StartFunc, _, Shutdown, Type, Modules}) ->
    #{id => Id, start => StartFunc, shutdown => Shutdown, type => Type, modules => Modules};
normalize_spec(#{id := _} = Spec) ->
    maps:remove(restart, Spec).
