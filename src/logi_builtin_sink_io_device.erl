%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in IO device sink
%%
%% This sink writes log messages to an IO device (e.g. standard output, file, etc)
%%
%% The default layout is `logi_builtin_layout_default:new()'.
%%
%% == NOTE ==
%% This module is provided for debugging/testing purposes only.
%% (e.g. Overload protection is missing)
%%
%% == EXAMPLE ==
%%
%% The default IO device is `standard_io':
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(foo), info).
%% > logi:info("hello world").
%% 2015-10-21 05:21:52.332 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
%%
%% Outputs to a file:
%% <pre lang="erlang">
%% > {ok, Fd} = file:open("/tmp/hoge", [write]).
%% > Sink = logi_builtin_sink_io_device:new(foo, [{io_device, Fd}]).
%% > {ok, _} = logi_channel:install_sink(Sink, info).
%% > logi:info("hello world").
%% > file:read_file("/tmp/hoge").
%% {ok,&lt;&lt;"2015-10-21 05:23:19.940 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world\n"&gt;&gt;}
%% </pre>
%%
%% Customizes message layout:
%% <pre lang="erlang">
%% > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[my_layout] " ++ Format ++ "\n", Data) end).
%% > Sink = logi_builtin_sink_io_device:new(foo, [{layout, Layout}]).
%% > {ok, _} = logi_channel:install_sink(Sink, info).
%% > logi:info("hello world").
%% [my_layout] hello world
%% </pre>
%% @end
-module(logi_builtin_sink_io_device).

-behaviour(logi_sink_writer).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4, get_writee/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Id, [])
-spec new(logi_sink:id()) -> logi_sink:sink().
new(Id) ->
    new(Id, []).

%% @doc Creates a new sink instance
%%
%% === DEFAULT VALUE ===
%% - io_device: `standard_io'
%% - layout: `logi_builtin_layout_default:new()'
%%
-spec new(logi_sink:id(), Options) -> logi_sink:sink() when
      Options :: [Option],
      Option  :: {io_device, io:device()}
               | {layout, logi_layout:layout()}.
new(Id, Options) ->
    _ = is_list(Options) orelse error(badarg, [Options]),

    IoDevice = proplists:get_value(io_device, Options, standard_io),
    Layout = proplists:get_value(layout, Options, logi_builtin_layout_default:new()),
    _ = is_pid(IoDevice) orelse is_atom(IoDevice) orelse error(badarg, [Options]),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Options]),

    logi_sink:from_writer(Id, logi_sink_writer:new(?MODULE, {Layout, IoDevice})).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, {Layout, IoDevice}) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    _ = io:put_chars(IoDevice, FormattedData),
    FormattedData.

%% @private
get_writee(IoDevice) when is_pid(IoDevice) ->
    IoDevice;
get_writee(IoDevice) ->
    whereis(IoDevice).
