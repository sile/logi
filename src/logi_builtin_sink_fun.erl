%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO:
%%
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
-export([write/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type write_fun() :: fun ((logi_context:context(), io:format(), logi_layout:data()) -> any()).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv install(Condition, Fun, [])
-spec install(logi_sink:condition(), write_fun()) -> logi_channel:install_sink_result().
install(Condition, Fun) -> install(Condition, Fun, []).

%% @doc Installs a sink
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
    _ = erlang:is_function(Fun, 3) orelse error(badarg, [Condition, Fun, Options]),
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    Sink = logi_sink:new(proplists:get_value(id, Options, ?MODULE), ?MODULE, Condition, Fun),
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
write(Context, Format, Data, Fun) ->
    Fun(Context, Format, Data).
