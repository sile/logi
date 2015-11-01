%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in IO device sink
%%
%% This sink writes log messages to an IO device (e.g. standard output, file, etc)
%%
%% The default layout is `logi_builtin_layout_simple:new()'.
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
-export([new/0, new/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type extra_data() :: io:device().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(standard_io)
-spec new() -> logi_sink:sink().
new() -> new(standard_io).

%% @doc Creates a new sink instance
-spec new(io:device()) -> logi_sink:sink().
new(IoDevice) ->
    _ = is_pid(IoDevice) orelse is_atom(IoDevice) orelse error(badarg, [IoDevice]),
    logi_sink:new(?MODULE, IoDevice).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec write(logi_context:context(), logi_layout:layout(), io:format(), logi_layout:data(), extra_data()) -> any().
write(Context, Layout, Format, Data, IoDevice) ->
    IoData = logi_layout:format(Context, Format, Data, Layout),
    io:put_chars(IoDevice, IoData).

%% @private
default_layout(_Extra) ->
    logi_builtin_layout_simple:new().
