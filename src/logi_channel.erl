%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A channel process
%% @private
-module(logi_channel).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([create/1]).
-export([delete/1]).
-export([which_channels/0]).
-export([default_channel/0]).

-export_type([id/0]).

%% -export([install_sink/2, install_sink/3]).
%% -export([uninstall_sink/2]).
%% -export([find_sink/2]).
%% -export([which_sinks/2]).
%% -export([set_condition/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
%% -export([select_sink/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(VALIDATE_AND_GET_CHANNEL_PID(ChannelId, Args),
        case is_atom(ChannelId) of
            false -> error(badarg, Args);
            true  -> case whereis(ChannelId) of
                         undefined -> error({channel_is_not_running, ChannelId}, Args);
                         ChannelPid -> ChannelPid
                     end
        end).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          id             :: logi:channel_id(),
          table          :: logi_appender_table:table(),
          appenders = [] :: appenders()
        }).

-type appenders() :: [{logi_appender:id(), lifetime_ref(), cancel_lifetime_fun(), logi_appender:appender()}].

-type lifetime_ref() :: undefined | reference().
-type cancel_lifetime_fun() :: fun (() -> any()).

-type id() :: atom().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new channel
%%
%% If the channel exists, nothing happens.
%%
%% TODO: badarg (ets or process name conflict)
-spec create(id()) -> ok.
create(Id) ->
    case logi_channel_sup:start_child(Id) of
        {ok, _} -> ok;
        _       ->
            case lists:member(Id, which_channels()) of
                true  -> ok;
                false -> error(badarg, [Id])
            end
    end.

%% @doc Deletes a channel
%%
%% If the channel does not exists, it will be silently ignored.
-spec delete(id()) -> ok.
delete(Id) when is_atom(Id) -> logi_channel_sup:stop_child(Id);
delete(Id)                  -> error(badarg, [Id]).

%% @doc Returns a list of all running channels
-spec which_channels() -> [id()].
which_channels() -> logi_channel_sup:which_children().

%% @doc The default channel
%%
%% The channel  is created automatically when `logi' application was started.
-spec default_channel() -> id().
default_channel() -> logi:default_logger().

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a channel process
-spec start_link(logi:channel_id()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id], []).

%% %% @doc Registers an appender
%% -spec register_appender(logi:channel_id(), logi_appender:appender(), Options) -> Result when
%%       Options :: #{
%%         lifetime  => timeout() | pid(),
%%         if_exists => error | ignore | supersede
%%        },
%%       Result :: {ok, OldAppender} | {error, Reason},
%%       OldAppender :: undefined | logi_appender:appender(),
%%       Reason :: {already_registered, logi_appender:appender()}.
%% register_appender(Id, Appender, Options) ->
%%     Args = [Id, Appender, Options],
%%     Defaults = #{lifetime => infinity, if_exists => error},
%%     case maps:merge(Defaults, Options) of
%%         #{if_exists := X} when X =/= error, X =/= ignore, X =/= supersede -> error(badarg, Args);
%%         #{lifetime := Lifetime, if_exists := IfExists}                    ->
%%             _ = is_valid_lifetime(Lifetime) orelse error(badarg, Args),
%%             Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Id, Args),
%%             gen_server:call(Pid, {register_appender, {Appender, Lifetime, IfExists}})
%%     end.

%% %% @doc Deregisters an appender
%% -spec deregister_appender(logi:channel_id(), logi_appender:id()) -> {ok, logi_appender:appender()} | error.
%% deregister_appender(Id, AppenderId) ->
%%     _ = is_atom(AppenderId) orelse error(badarg, [Id, AppenderId]),
%%     Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Id, [Id, AppenderId]),
%%     gen_server:call(Pid, {deregister_appender, AppenderId}).

%% %% @doc TODO
%% -spec find_appender(logi:channel_id(), logi_appender:id()) -> {ok, logi_appender:appender()} | error.
%% find_appender(Id, AppenderId) ->
%%     _ = is_atom(AppenderId) orelse error(badarg, [Id, AppenderId]),
%%     Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Id, [Id, AppenderId]),
%%     gen_server:call(Pid, {find_appender, AppenderId}).

%% %% @doc Returns a list of registered appenders
%% -spec which_appenders(logi:channel_id()) -> [logi_appender:id()].
%% which_appenders(Id) ->
%%     _ = ?VALIDATE_AND_GET_CHANNEL_PID(Id, [Id]),
%%     logi_appender_table:which_appenders(Id).

%% %% @doc TODO
%% -spec set_condition(logi:channel_id(), logi_appender:id(), logi_appender:condition()) -> {ok, logi_appender:condition()} | error.
%% set_condition(Id, AppenderId, Condition) ->
%%     _ = is_atom(AppenderId) orelse error(badarg, [Id, AppenderId, Condition]),
%%     _ = logi_appender:is_valid_condition(Condition) orelse error(badarg, [Id, AppenderId, Condition]),
%%     Pid = ?VALIDATE_AND_GET_CHANNEL_PID(Id, [Id, AppenderId, Condition]),
%%     gen_server:call(Pid, {set_condition, {AppenderId, Condition}}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Id]) ->
    _ = process_flag(trap_exit, true),
    State =
        #?STATE{
            id    = Id,
            table = logi_appender_table:new(Id)
           },
    {ok, State}.

%% @private
handle_call({register_appender,   Arg}, _, State) -> handle_register_appender(Arg, State);
handle_call({deregister_appender, Arg}, _, State) -> handle_deregister_appender(Arg, State);
handle_call({find_appender,       Arg}, _, State) -> handle_find_appender(Arg, State);
handle_call({set_condition,       Arg}, _, State) -> handle_set_condition(Arg, State);
handle_call(_, _, State)                          -> {noreply, State}.

%% @private
handle_cast(_, State) -> {noreply, State}.

%% @private
handle_info({'DOWN', Ref, _, _, _}, State) -> handle_down(Ref, State);
handle_info(_, State) ->
    %% TODO: logi:warning
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_register_appender(Arg, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Arg         :: {logi_appender:appender(), Lifetime, IfExists},
      Lifetime    :: timeout() | pid(),
      IfExists    :: error | ignore | supersede,
      Result      :: {ok, OldAppender} | {error, Reason},
      OldAppender :: undefined | logi_appender:appender(),
      Reason      :: {already_registered, logi_appender:appender()}.
handle_register_appender({Appender, Lifetime, IfExists}, State0) ->
    {OldAppender, OldCancelLifetimeFun, Appenders0} = take_appender(logi_appender:get_id(Appender), State0#?STATE.appenders),
    case OldAppender =:= undefined orelse IfExists =:= supersede of
        false ->
            case IfExists of
                error  -> {reply, {error, {already_registered, OldAppender}}, State0};
                ignore -> {reply, {ok, OldAppender}, State0}
            end;
        true ->
            _  = OldCancelLifetimeFun(),
            ok = logi_appender_table:register(State0#?STATE.table, Appender, OldAppender),
            {LifetimeRef, CancelLifetimeFun} = set_lifetime(Lifetime),
            Appenders1 = [{logi_appender:get_id(Appender), LifetimeRef, CancelLifetimeFun, Appender} | Appenders0],
            State1 = State0#?STATE{appenders = Appenders1},
            {reply, {ok, OldAppender}, State1}
    end.

-spec handle_deregister_appender(logi_appender:id(), #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: undefined | logi_appender:appender().
handle_deregister_appender(AppenderId, State0) ->
    case take_appender(AppenderId, State0#?STATE.appenders) of
        {undefined, _, _}                        -> {reply, error, State0};
        {Appender, CancelLifetimeFun, Appenders} ->
            _ = CancelLifetimeFun(),
            State1 = State0#?STATE{appenders = Appenders},
            {reply, {ok, Appender}, State1}
    end.

-spec handle_find_appender(logi_appender:id(), #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: {ok, logi_appender:appender()} | error.
handle_find_appender(AppenderId, State) ->
    case lists:keyfind(AppenderId, 1, State#?STATE.appenders) of
        false               -> {reply, error, State};
        {_, _, _, Appender} -> {reply, {ok, Appender}, State}
    end.

-spec handle_set_condition({logi_appender:id(), logi_appender:condition()}, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Result :: {ok, logi_appender:condition()} | error.
handle_set_condition({AppenderId, Condition}, State0) ->
    case lists:keytake(AppenderId, 1, State0#?STATE.appenders) of
        false                                            -> {reply, error, State0};
        {value, Entry = {_, _, _, Appender0}, Appenders} ->
            Appender1 = logi_appender:from_map(maps:merge(logi_appender:to_map(Appender0), #{condition => Condition})),
            ok = logi_appender_table:register(State0#?STATE.table, Appender1, Appender0),
            State1 = State0#?STATE{appenders = [setelement(4, Entry, Appender1) | Appenders]},
            {reply, {ok, logi_appender:get_condition(Appender0)}, State1}
    end.

-spec handle_down(reference(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Ref, State0) ->
    case lists:keytake(Ref, 2, State0#?STATE.appenders) of
        false                                   -> {noreply, State0};
        {value, {_, _, _, Appender}, Appenders} ->
            ok = logi_appender_table:deregister(State0#?STATE.table, Appender),
            State1 = State0#?STATE{appenders = Appenders},
            {noreply, State1}
    end.

-spec take_appender(logi_appender:id(), appenders()) -> {MaybeAppender, cancel_lifetime_fun(), appenders()} when
      MaybeAppender :: undefined | logi_appender:appender().
take_appender(AppenderId, Appenders0) ->
    case lists:keytake(AppenderId, 1, Appenders0) of
        false                                                    -> {undefined, fun () -> ok end, Appenders0};
        {value, {_, _, CancelLifetimeFun, Appender}, Appenders1} -> {Appender, CancelLifetimeFun, Appenders1}
    end.

-spec set_lifetime(timeout() | pid()) -> {lifetime_ref(), cancel_lifetime_fun()}.
set_lifetime(infinity)             -> {undefined, fun () -> ok end};
set_lifetime(Pid) when is_pid(Pid) -> {monitor(process, Pid), fun erlang:demonitor/1};
set_lifetime(Time)                 ->
    TimeoutRef = make_ref(),
    TimerRef = erlang:send_after(Time, self(), {'DOWN', TimeoutRef, timeout, undefined, timeout}),
    {TimeoutRef, fun () -> erlang:cancel_timer(TimerRef, [{async, true}]) end}.

%% -spec is_valid_lifetime(timeout() | pid() | term()) -> boolean().
%% is_valid_lifetime(infinity)                                                               -> true;
%% is_valid_lifetime(Pid) when is_pid(Pid)                                                   -> true;
%% is_valid_lifetime(Timeout) when is_integer(Timeout), Timeout >= 0, Timeout < 16#100000000 -> true;
%% is_valid_lifetime(_)                                                                      -> false.
