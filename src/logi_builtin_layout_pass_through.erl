%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in no-op layout
%%
%% This layout simply returns original `Format' and `Data' as tuple format (i.e. `{Format, Data}').
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
%%
%% > Context = logi_context:new(sample_log, info).
%% > Layout = logi_builtin_layout_pass_through:new().
%% > logi_layout:format(Context, "hello ~s", [world], Layout).
%% {"hello ~s", [world]}
%% </pre>
-module(logi_builtin_layout_pass_through).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new layout instance
-spec new() -> logi_layout:layout({io:format(), logi_layout:data()}).
new() -> logi_layout:new(?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
format(_, Format, Data, _) -> {Format, Data}.
