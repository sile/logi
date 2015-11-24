%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_sink_agent).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([suspend_sink/2]).
-export([resume_sink/1]).
-export([update_sink/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec suspend_sink(logi_sink:controlling_process(), Reason::term()) -> ok.
suspend_sink(Pid, Reason) ->
    _ = Pid ! {'SUSPEND_SINK', self(), Reason},
    ok.

-spec resume_sink(logi_sink:controlling_process()) -> ok.
resume_sink(Pid) ->
    _ = Pid ! {'RESUME_SINK', self()},
    ok.

-spec update_sink(logi_sink:controlling_process(), logi_sink:sink()) -> ok.
update_sink(Pid, Sink) ->
    _ = logi_sink:is_sink(Sink) orelse error(badarg, [Pid, Sink]),
    _ = Pid ! {'UPDATE_SINK', self(), Sink},
    ok.
