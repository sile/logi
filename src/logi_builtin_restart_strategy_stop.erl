%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_builtin_restart_strategy_stop).

-behaviour(logi_restart_strategy).

-export([new/0]).

-export([next/1]).


new() ->
    logi_restart_strategy:new(?MODULE).

next(_) ->
    stop.
