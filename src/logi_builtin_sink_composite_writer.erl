%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A built-in composite sink process and writer
%%
%% == NOTE ==
%% This module is provided for debuging/testing purposes only.
%%
%% @end
-module(logi_builtin_sink_composite_writer).

-behaviour(logi_sink_writer).
-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([get_children/1]).
-export([get_active_writer/1]).
-export([set_active_writer/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4, get_writee/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-record(child,
        {
          sink   :: logi_sink:sink(),
          sup    :: logi_sink_proc:sink_sup(),
          writer :: logi_sink_writer:writer() | undefined
        }).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          children = [] :: [#child{}],
          active        :: logi_sink_writer:writer() | undefined
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new process
-spec start_link([logi_sink:sink()]) -> {ok, pid()} | {error, Reason::term()}.
start_link(Children = [_|_]) ->
    gen_server:start_link(?MODULE, Children, []).

%% @doc Returns a list of children
-spec get_children(pid()) -> [logi_sink:sink()].
get_children(Pid) ->
    gen_server:call(Pid, get_children).

%% @doc Gets the active child writer
-spec get_active_writer(pid()) -> logi_sink_writer:writer() | undefined.
get_active_writer(Pid) ->
    gen_server:call(Pid, get_active_writer).

%% @doc Sets the `Nth' cihld to be active
-spec set_active_writer(pid(), pos_integer()) -> ok.
set_active_writer(Pid, Nth) ->
    gen_server:cast(Pid, {set_active_writer, Nth}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, Pid) ->
    case get_active_writer(Pid) of
        undefined -> [];
        Writer    -> logi_sink_writer:write(Context, Format, Data, Writer)
    end.

%% @private
get_writee(Pid) ->
    case get_active_writer(Pid) of
        undefined -> undefined;
        Writer    -> logi_sink_writer:get_writee(Writer)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init(Sinks) ->
    Start =
        fun (_, {error, Reason}) ->
                {error, Reason};
            (Sink, {ok, Acc}) ->
                case logi_sink_proc:start_child(Sink) of
                    {error, Reason} -> {error, Reason};
                    {ok, SinkSup}   ->
                        true = link(SinkSup),
                        Writer = logi_sink_proc:recv_writer_from_child(SinkSup, 100),
                        Child = #child{sink = Sink, sup = SinkSup, writer = Writer},
                        {ok, [Child | Acc]}
                end
        end,
    case lists:foldl(Start, {ok, []}, Sinks) of
        {error, Reason} -> {stop, Reason};
        {ok, Children0} ->
            Children1 = [Active | _] = lists:reverse(Children0),
            State =
                #?STATE{
                    children = Children1,
                    active   = Active#child.writer
                   },
            ok = logi_sink_proc:send_writer_to_parent(State#?STATE.active),
            {ok, State}
    end.

%% @private
handle_call(get_children, _From, State) ->
    {reply, [C#child.sink || C <- State#?STATE.children], State};
handle_call(get_active_writer, _From, State) ->
    {reply, State#?STATE.active, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({set_active_writer, Nth}, State) ->
    Active = (lists:nth(Nth, State#?STATE.children))#child.writer,
    ok = logi_sink_proc:send_writer_to_parent(Active),
    {noreply, State#?STATE{active = Active}};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({sink_writer, SinkSup, Writer}, State) ->
    Child0 = lists:keyfind(SinkSup, #child.sup, State#?STATE.children),
    Child1 = Child0#child{writer = Writer},
    Children = lists:keystore(SinkSup, #child.sup, State#?STATE.children, Child1),
    _ = Child0#child.writer =:= State#?STATE.active andalso logi_sink_proc:send_writer_to_parent(Child1#child.writer),
    {noreply, State#?STATE{children = Children}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
