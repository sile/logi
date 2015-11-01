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
-export([install/2, install/3]).
-export([uninstall/0, uninstall/1]).

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
%% @equiv install(Condition, Fun, [])
-spec install(logi_sink:condition(), write_fun()) -> logi_channel:install_sink_result().
install(Condition, Fun) -> install(Condition, Fun, []).

%% @doc Installs a sink which writes log messages by `Fun'
%%
%% The default value of `Options': <br />
%% - id: `logi_builtin_sink_fun' <br />
%% - channel: `logi_channel:default_channel()' <br />
-spec install(logi_sink:condition(), write_fun(), Options) -> logi_channel:install_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}
               | logi_channel:install_sink_option().
install(Condition, Fun, Options) ->
    _ = erlang:is_function(Fun, 4) orelse error(badarg, [Condition, Fun, Options]),
    Sink = logi_sink:new(?MODULE, Fun),
    logi_channel:install_sink(Condition, Sink, Options).

%% @equiv uninstall([])
-spec uninstall() -> logi_channel:uninstall_sink_result().
uninstall() -> uninstall([]).

%% @doc Uninstalls a sink
%%
%% The default value of `Options': <br />
%% - id: `logi_builtin_sink_fun' <br />
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
write(Context, Layout, Format, Data, Fun) ->
    Fun(Context, Layout, Format, Data).

%% @private
default_layout(_Extra) ->
    logi_builtin_layout_simple:new().
