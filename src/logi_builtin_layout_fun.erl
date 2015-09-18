%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
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
%% Therefore if the format function (`format_fun/3') of the layout is a huge size anonymous function,
%% all log issuers which use the channel will have to pay a non negligible cost to load it.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > Context = logi_context:new(sample_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}).
%% > FormatFun = fun (_, Format, Data) -> io_lib:format("EXAMPLE: " ++ Format, Data) end.
%% > Layout = logi_builtin_layout_fun:new(FormatFun).
%% > lists:flatten(logi_layout:format(Context, "Hello ~s", ["World"], Layout)).
%% "EXAMPLE: Hello World"
%% </pre>
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
-type format_fun() :: fun ((logi_context:context(), io:format(), logi_layout:data()) -> iodata()).
%% A log message formatting function

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a layout which formats log messages by `Fun'
-spec new(format_fun()) -> logi_layout:layout(format_fun()).
new(Fun) ->
    _ = erlang:is_function(Fun, 3) orelse error(badarg, [Fun]),
    {?MODULE, Fun}.

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(logi_context:context(), io:format(), logi_layout:data(), format_fun()) -> iodata().
format(Context, Format, Data, Fun) -> Fun(Context, Format, Data).
