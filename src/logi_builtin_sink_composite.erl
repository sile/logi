%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in composite sink
%%
%% == NOTE ==
%% This module is provided for debuging/testing purposes only.
%%
%% @end
-module(logi_builtin_sink_composite).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creats a new sink instance
-spec new(logi_sink:id(), [logi_sink:sink()]) -> logi_sink:sink().
new(Id, Children) ->
    _ = is_list(Children) andalso lists:all(fun logi_sink:is_sink/1, Children) orelse error(badarg, [Id, Children]),
    logi_sink:new(#{id => Id, start => {logi_builtin_sink_composite_writer, start_link, [Children]}}).
