%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% TODO: support handlers hot code swap
-module(logi_event_manager).

-behaviour(gen_server).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/0, start_link/1,
         stop/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(state,
        {
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% TODO: doc
-spec start_link(logi:event_manager_name()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(ManagerName) ->
    gen_server:start_link(ManagerName, ?MODULE, [], []).

%% TOD: doc
-spec stop(logi:event_manager_ref()) -> ok.
stop(ManagerRef) ->
    gen_server:cast(ManagerRef, stop).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([]) ->
    State = #state{},
    {ok, State}.

%% @hidden
handle_call(_Request, _From, State) ->
    %% TODO: logging
    {noreply, State}.

%% @hidden
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    %% TODO: logging
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    %% TODO: logging
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
