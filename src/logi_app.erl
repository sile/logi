%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc application module
%% @private
-module(logi_app).

-behaviour(application).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([start/2, stop/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
start(_StartType, _StartArgs) ->
    Result = logi_sup:start_link(),
    ok = case Result of
             {ok, _} -> logi_channel:create(logi_channel:default_channel());
             _       -> ok
         end,
    Result.

%% @private
stop(_State) ->
    ok.
