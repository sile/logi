%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in stateless filter which filters log messages by an arbitrary user defined function
%%
%% == NOTE ==
%% This module is mainly supposed to be used for ddebugging/testing purposes.
%%
%% If you want to set a particular filter to a lot of logger instances,
%% it is recommended to define a specified filter for efficiency reasons.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% > Sink = logi_builtin_sink_fun:new(foo, fun (_, Format, Data) -> io:format(Format ++ "\n", Data) end).
%% > {ok, _} = logi_channel:install_sink(Sink, info).
%%
%% > FilterFun = fun (C) -> not maps:get(discard, logi_context:get_metadata(C), false) end.
%% > Logger = logi:new([{filter, logi_builtin_filter_fun:new(FilterFun)}]).
%% > logi:save_as_default(Logger).
%%
%% > logi:info("hello world", [], [{messages, #{discard => false}}]). % passed
%% hello world
%%
%% > logi:info("hello world", [], [{metadata, #{discard => true}}]). % discarded
%% % No output: the log message was discarded by the filter
%% </pre>
%% @end
-module(logi_builtin_filter_fun).

-behaviour(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).

-export_type([filter_fun/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% `logi_filter` Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([filter/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type filter_fun() :: fun ((logi_context:context()) -> boolean()).
%% A log messages filter function

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a filter which filters log messages by `FilterFun'
-spec new(filter_fun()) -> logi_filter:filter().
new(FilterFun) ->
    _ = erlang:is_function(FilterFun, 1) orelse error(badarg, [FilterFun]),
    logi_filter:new(?MODULE, FilterFun).

%%----------------------------------------------------------------------------------------------------------------------
%% `logi_filter` Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec filter(logi_context:context(), filter_fun()) -> boolean().
filter(Context, FilterFun) -> FilterFun(Context).
