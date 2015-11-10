%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
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
%% > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_io_device:new()).
%% > logi:info("hello world").
%% 2015-10-21 05:21:52.332 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
%%
%% Outputs to a file:
%% <pre lang="erlang">
%% > {ok, Fd} = file:open("/tmp/hoge", [write]).
%% > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_io_device:new(Fd), [{if_exists, supersede}]).
%% > logi:info("hello world").
%% > file:read_file("/tmp/hoge").
%% {ok,&lt;&lt;"2015-10-21 05:23:19.940 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world\n"&gt;&gt;}
%% </pre>
%%
%% Customizes message layout:
%% <pre lang="erlang">
%% > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[my_layout] " ++ Format ++ "\n", Data) end).
%% > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_io_device:new(), [{layout, Layout}, {if_exists, supersede}]).
%% > logi:info("hello world").
%% [my_layout] hello world
%% </pre>
-module(logi_builtin_sink_io_device).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, new/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new([])
-spec new() -> logi_sink:sink().
new() -> new([]).

%% @doc Creates a new sink instance
%%
%% TODO: default value
-spec new(Options) -> logi_sink:sink() when
      Options :: [Option],
      Option  :: {io_device, io:device()}
               | {layout, logi_layout:layout()}.
new(Options) ->
    _ = is_list(Options) orelse error(badarg, [Options]),

    IoDevice = proplists:get_value(io_device, Options, standard_io),
    Layout = proplists:get_value(layout, Options, logi_builtin_layout_default:new()),
    _ = is_pid(IoDevice) orelse is_atom(IoDevice) orelse error(badarg, [Options]),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Options]),

    logi_sink:new(?MODULE, Layout, IoDevice).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(_Context, FormattedData, IoDevice) ->
    io:put_chars(IoDevice, FormattedData).
