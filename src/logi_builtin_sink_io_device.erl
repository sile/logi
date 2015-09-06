%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in IO device sink
%%
%% NOTE: This module is provided for debuging/testing purposes only.
%% (e.g. Overload protection is missing)
%%
%% ```
%% %%%
%% %%% Usage Example
%% %%%
%%
%% %%
%% %% 1. The default IO device is `standard_io'
%% %%
%% > logi_builtin_sink_io_device:install(info).
%% > logi:info("Hello World").
%% # TODO: show output
%% > logi_builtin_sink_io_device:uninstall().
%%
%% %%
%% %% 2. Output to a file
%% %%
%% > {ok, Fd} = file:open("/tmp/hoge", [write]).
%% > logi_builtin_sink_io_device:install(info, [{io_device, Fd}]).
%% > logi:info("Hello World").
%% > logi_builtin_sink_io_device:uninstall().
%% > file:read_file("/tmp/hoge").
%% # TODO: show result
%% '''
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
%% - id: `logi_builtin_sink_null' <br />
%% - channel: `logi_channel:default_channel()' <br />
%% - io_device: `standard_io' <br />
%% - layout: `logi_builtin_layout_simple' <br />
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
%% - id: `logi_builtin_sink_null' <br />
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
-spec write(logi_context:context(), io:format(), [term()], extra_data()) -> any().
write(Context, Format, FormatArgs, {IoDevice, Layout}) ->
    IoData = logi_layout:format(Context, Format, FormatArgs, Layout),
    io:fwrite(IoDevice, IoData).
