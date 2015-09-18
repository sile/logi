%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in null sink
%%
%% This sink discards all log messages.
%%
%% This module is still useful as a simplest referential implementation of the `logi_sink' behaviour.
%%
%% ```
%% %%%
%% %%% Usage Example
%% %%%
%%
%% %%
%% %% 1. Default Channel
%% %%
%% > logi_builtin_sink_null:install(info).
%% > logi:info("Hello World").
%% > logi_builtin_sink_null:uninstall().
%%
%% %%
%% %% 2. Non-default Channel
%% %%
%% > logi_channel:create(null_channel).
%% > logi_builtin_sink_null:install(info, [{channel, null_channel}]).
%% > logi:info("Hello World").
%% > logi_builtin_sink_null:uninstall([{channel, null_channel}]).
%% '''
-module(logi_builtin_sink_null).

-behaviour(logi_sink).

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
-spec install(logi_sink:condition(), Options) -> logi_channel:install_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}
               | logi_channel:install_sink_option().
install(Condition, Options) ->
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    Sink = logi_sink:new(proplists:get_value(id, Options, ?MODULE), ?MODULE, Condition),
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
write(_Context, _Format, _Data, _Extra) ->
    ok.
