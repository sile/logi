%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_builtin_restart_strategy_constant).

-behaviour(logi_restart_strategy).

-export([new/1]).

-export([next/1]).


new(Time) ->
    logi_restart_strategy:new(?MODULE, Time).

next(Time) ->
    {ok, Time, Time}.
