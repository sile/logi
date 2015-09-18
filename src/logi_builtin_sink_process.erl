%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in process sink
%%
%% NOTE: This module is provided for debuging/testing purposes only.
%% (e.g. Overload protection is missing)
%%
%% ```
%% %%%
%% %%% Usage Example
%% %%%
%% > logi_builtin_sink_process:install(info, self(), [{extra, hoge}]).
%% > logi:info("Hello World").
%% > flush().
%% # TODO: show output
%% > logi_builtin_sink_process:uninstall().
%% '''
%%
%% TODO: change to logi_builtin_sink_fun
-module(logi_builtin_sink_process).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([install/2, install/3]).
-export([uninstall/0, uninstall/1]).

-export_type([dst/0, extra/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type extra_data() :: {dst(), extra()}.
-type dst() :: pid()
             | port()
             | (RegName :: atom())
             | {RegName :: atom(), Node :: node()}.
-type extra() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv install(Condition, Dst, [])
-spec install(logi_sink:condition(), dst()) -> logi_channel:install_sink_result().
install(Condition, Dst) -> install(Condition, Dst, []).

%% @doc Installs a sink
%%
%% When `logi_sink:write/4' is invoked,
%% the message `{'LOGI_MSG', self(), logi_context:context(), io:format(), [term()], extra()}' will send to `Dst' process.
%%
%% The default value of `Options': <br />
%% - id: `logi_builtin_sink_null' <br />
%% - channel: `logi_channel:default_channel()' <br />
%% - extra: `undefined' <br />
-spec install(logi_sink:condition(), dst(), Options) -> logi_channel:install_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}
               | {extra, extra()}
               | logi_channel:install_sink_option().
install(Condition, Dst, Options) ->
    _ = is_dst(Dst) orelse error(badarg, [Condition, Dst, Options]),
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    Extra = proplists:get_value(extra, Options, undefined),
    Sink = logi_sink:new(proplists:get_value(id, Options, ?MODULE), ?MODULE, Condition, {Dst, Extra}),
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
-spec write(logi_context:context(), io:format(), logi_layout:data(), extra_data()) -> any().
write(Context, Format, Data, {Dst, Extra}) ->
    Dst ! {'LOGI_MSG', self(), Context, Format, Data, Extra}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_dst(dst() | term()) -> boolean().
is_dst(X)      when is_pid(X)              -> true;
is_dst(X)      when is_port(X)             -> true;
is_dst(X)      when is_atom(X)             -> true;
is_dst({X, N}) when is_atom(X), is_atom(N) -> true;
is_dst(_)                                  -> false.
