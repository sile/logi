%% @private
-module(logi_sink_noop).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-spec start_link(logi_sink:parent(), logi_sink_writer:writer()) -> {ok, pid()}.
start_link(Writer) ->
    gen_server:start_link(?MODULE, Writer, []).

%% @private
init(Writer) ->
    ok = logi_sink_proc:send_writer(Writer),
    {ok, []}.

%% @private
handle_call(Request, From, State) ->
    %% TODO: ignore
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
