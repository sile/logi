%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in sink which consumes log messages by an arbitrary user defined function
%%
%% The default layout is `logi_builtin_layout_simple:new()'.
%%
%% == NOTE ==
%% This module is provided for debuging/testing purposes only.
%%
%% A sink is stored into a logi_channel's ETS.
%% Then it will be loaded every time a log message is issued.
%% Therefore if the write function (`write_fun/0') of the sink is a huge size anonymous function,
%% all log issuers which use the channel will have to pay a non negligible cost to load it.
%%
%% And there is no overload protection.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > WriteFun = fun (_, _, Format, Data) -> io:format("[CONSUMED] " ++ Format ++ "\n", Data) end.
%% > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun).
%% > logi:info("hello world").
%% [CONSUMED] hello world
%% </pre>
-module(logi_builtin_sink_fun).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).

-export_type([write_fun/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type write_fun() :: fun ((logi_context:context(), logi_layout:layout(), io:format(), logi_layout:data()) -> any()).
%% A function which is used to consume log messages issued by `logi'

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creats a new sink instance
-spec new(write_fun()) -> logi_sink:sink().
new(Fun) ->
    _ = erlang:is_function(Fun, 4) orelse error(badarg, [Fun]),
    logi_sink:new(?MODULE, Fun).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, Fun) ->
    Fun(Context, Layout, Format, Data).

%% @private
default_layout(_Extra) ->
    logi_builtin_layout_simple:new().