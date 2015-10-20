%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in IO device sink
%%
%% This sink writes log messages to an IO device (e.g. standard output, file, etc)
%%
%% == NOTE ==
%% This module is provided for debugging/testing purposes only.
%% (e.g. Overload protection is missing)
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > application:set_env(logi, warn_no_parse_transform, false). % Suppresses noisy warnings
%%
%% %%
%% %% 1. The default IO device is `standard_io'
%% %%
%% > logi_builtin_sink_io_device:install(info).
%% > logi:info("hello world").
%% 2015-10-21 05:21:52.332 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%%
%% %%
%% %% 2. Outputs to a file
%% %%
%% > {ok, Fd} = file:open("/tmp/hoge", [write]).
%% > logi_builtin_sink_io_device:install(info, [{io_device, Fd}, {if_exists, supersede}]).
%% > logi:info("hello world").
%% > file:read_file("/tmp/hoge").
%% {ok,&lt;&lt;"2015-10-21 05:23:19.940 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world\n"&gt;&gt;}
%%
%% %%
%% %% 3. Customizes message layout
%% %%
%% > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[my_layout] " ++ Format ++ "\n", Data) end).
%% > logi_builtin_sink_io_device:install(info, [{layout, Layout}, {if_exists, supersede}]).
%% > logi:info("hello world").
%% [my_layout] hello world
%% </pre>
-module(logi_builtin_sink_io_device).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([install/1, install/2]).
-export([uninstall/0, uninstall/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type extra_data() :: {io:device(), logi_layout:layout()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv install(Condition, [])
-spec install(logi_sink:condition()) -> logi_channel:install_sink_result().
install(Condition) -> install(Condition, []).

%% @doc Installs a sink
%%
%% The default value of `Options': <br />
%% - id: `logi_builtin_sink_io_device' <br />
%% - channel: `logi_channel:default_channel()' <br />
%% - io_device: `standard_io' <br />
%% - layout: `logi_builtin_layout_simple:new()' <br />
-spec install(logi_sink:condition(), Options) -> logi_channel:install_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}
               | {io_device, io:device()}
               | {layout, logi_layout:layout()}
               | logi_channel:install_sink_option().
install(Condition, Options) ->
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    IoDevice = proplists:get_value(io_device, Options, standard_io),
    Layout = proplists:get_value(layout, Options, logi_builtin_layout_simple),
    _ = is_pid(IoDevice) orelse is_atom(IoDevice) orelse error(badarg, [Condition, Options]),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Condition, Options]),

    Sink = logi_sink:new(proplists:get_value(id, Options, ?MODULE), ?MODULE, Condition, {IoDevice, Layout}),
    logi_channel:install_sink(Channel, Sink, Options).

%% @equiv uninstall([])
-spec uninstall() -> logi_channel:uninstall_sink_result().
uninstall() -> uninstall([]).

%% @doc Uninstalls a sink
%%
%% The default value of `Options': <br />
%% - id: `logi_builtin_sink_io_device' <br />
%% - channel: `logi_channel:default_channel()' <br />
-spec uninstall(Options) -> logi_channel:uninstall_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}.
uninstall(Options) ->
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    SinkId = proplists:get_value(id, Options, ?MODULE),
    logi_channel:uninstall_sink(Channel, SinkId).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec write(logi_context:context(), io:format(), logi_layout:data(), extra_data()) -> any().
write(Context, Format, Data, {IoDevice, Layout}) ->
    IoData = logi_layout:format(Context, Format, Data, Layout),
    io:put_chars(IoDevice, IoData).
