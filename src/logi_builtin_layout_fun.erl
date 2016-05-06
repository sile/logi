%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in layout which formats log messages by an arbitrary user defined function
%%
%% This layout formats log messages by `format_fun/0' which was specified by the argument of {@link new/1}.
%%
%% == NOTE ==
%% This module is provided for debuging/testing purposes only.
%%
%% A layout will be stored into a logi_channel's ETS.
%% Then it will be loaded every time a log message is issued.
%% Therefore if the format function (`format_fun/0') of the layout is a huge size anonymous function,
%% all log issuers which use the channel will have to pay a non negligible cost to load it.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% > Context = logi_context:new(sample_log, info).
%% > FormatFun = fun (_, Format, Data) -> io_lib:format("EXAMPLE: " ++ Format, Data) end.
%% > Layout = logi_builtin_layout_fun:new(FormatFun).
%% > lists:flatten(logi_layout:format(Context, "Hello ~s", ["World"], Layout)).
%% "EXAMPLE: Hello World"
%% </pre>
%%
%% A layout used by a sink can be specified at the time of installing the sink:
%% <pre lang="erlang">
%% > Layout0 = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[LAYOUT_0] " ++ Format ++ "\n", Data) end).
%% > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(foo, [{layout, Layout0}]), info).
%% > logi:info("hello world").
%% [LAYOUT_0] hello world
%%
%% > Layout1 = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[LAYOUT_1] " ++ Format ++ "\n", Data) end).
%% > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(bar, [{layout, Layout1}]), info).
%% > logi:info("hello world").
%% [LAYOUT_0] hello world
%% [LAYOUT_1] hello world
%% </pre>
%% @end
-module(logi_builtin_layout_fun).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).

-export_type([format_fun/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type format_fun() :: fun ((logi_context:context(), io:format(), logi_layout:data()) -> logi_layout:formatted_data()).
%% A log message formatting function

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a layout which formats log messages by `FormatFun'
-spec new(format_fun()) -> logi_layout:layout().
new(FormatFun) ->
    _ = erlang:is_function(FormatFun, 3) orelse error(badarg, [FormatFun]),
    logi_layout:new(?MODULE, FormatFun).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(logi_context:context(), io:format(), logi_layout:data(), format_fun()) -> logi_layout:formatted_data().
format(Context, Format, Data, Fun) -> Fun(Context, Format, Data).
