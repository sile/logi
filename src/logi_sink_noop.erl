%% @private
-module(logi_sink_noop).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start_link(logi_sink:parent(), logi_sink_writer:writer()) -> {ok, pid()}.
start_link(Parent, Writer) ->
    gen_server:start_link(?MODULE, [Parent, Writer], []).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          writer :: logi_sink_writer:writer()
        }).

%% @private
init([Parent, Writer]) ->
    ok = logi_sink:notify_started(Parent, Writer),
    State = #?STATE{writer = Writer},
    {ok, State}.

%% @private
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
