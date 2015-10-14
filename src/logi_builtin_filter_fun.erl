%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO
-module(logi_builtin_filter_fun).

-behaviour(logi_filter).

-export([new/1]).
-export([filter/2]).

new(Fun) ->
    logi_filter:new(?MODULE, Fun).

filter(Context, Fun) ->
    Fun(Context).
