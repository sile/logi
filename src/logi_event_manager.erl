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
         stop/1,
         get_handlers/3,
         add_handler/5,
         is_event_manager_ref/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          handlers = [] :: handlers()
        }).

-type handler_id() :: module() | {module(), term()}.

-type handler() :: {module(), term()}.

-type handlers() :: [{logi:severity(), [logi:metadata_entry()], module(), term()}]. % TODO: ordset

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

-spec is_event_manager_ref(logi:event_manager_ref() | term()) -> boolean().
is_event_manager_ref(MaybeManagerRef) ->
    is_atom(MaybeManagerRef) orelse is_pid(MaybeManagerRef).

-spec get_handlers(logi:event_manager_ref(), logi:severity(), [logi:metadata_entry()]) -> [handler()].
get_handlers(ManagerRef, Severity, MeataData) ->
    gen_server:call(ManagerRef, {get_handlers, {Severity, MeataData}}).

-spec add_handler(logi:event_manager_ref(), logi:severity(), [logi:metadata_entry()], handler_id(), term()) ->
                         ok | {error, Reason} when
      Reason :: already_exists | term().
add_handler(ManagerRef, Severity, Conditions, Handler, Arg) ->
    gen_server:call(ManagerRef, {add_handler, {Severity, lists:ukeysort(1, Conditions), Handler, Arg}}).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([]) ->
    State = #state{},
    {ok, State}.

%% @hidden
handle_call({get_handlers, Arg}, _From, State) ->
    {reply, do_get_handlers(Arg, State), State};
handle_call({add_handler, Arg}, _From, State) ->
    {Result, State2} = do_add_handler(Arg, State),
    {reply, Result, State2};
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

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec do_add_handler({logi:severity(), [logi:metadata_entry()], handler_id(),  term()}, #state{}) -> {Result, #state{}} when
      Result :: ok | {error, Reason},
      Reason :: already_exists | term().
do_add_handler({Severity, Conditions, HandlerId, Arg}, State) ->
    case lists:keymember(HandlerId, 3, State#state.handlers) of
        true  -> {{error, already_exists}, State};
        false ->
            Handler = {Severity, Conditions, HandlerId, Arg},
            {ok, State#state{handlers = lists:keymerge(1, [Handler], State#state.handlers)}}
    end.    

-spec do_get_handlers({logi:severity(), [logi:metadata_entry()]}, #state{}) -> [handler()].
do_get_handlers({Severity, MetaData}, State) ->
    #state{handlers = Handlers} = State,
    filter_handlers(Severity, MetaData, Handlers).

-spec filter_handlers(logi:severity(), [logi:metadata_entry()], handlers()) -> [handler()].
filter_handlers(Severity, MetaData, Handlers) ->
    Handlers2 =
        lists:dropwhile(fun ({Level, _, _, _}) -> severity_less_than(Level, Severity) end,
                        Handlers),
    Handlers3 =
        lists:filter(fun ({_, Conditions, _, _}) -> is_conditions_match(Conditions, MetaData) end,
                     Handlers2),
    [case Module of
         {M, _} -> {M, Arg};
         _      -> {Module, Arg}
     end || {_, _, Module, Arg} <- Handlers3].

-spec is_conditions_match([logi:metadata_entry()], [logi:metadata_entry()]) -> boolean().
is_conditions_match(Conditions, MetaData) ->
    ordsets:is_subset(Conditions, MetaData).

-spec severity_less_than(logi:severity(), logi:severity()) -> boolean().
severity_less_than(S1, S2) ->
    severity_to_int(S1) < severity_to_int(S2).

-spec severity_to_int(logi:severity()) -> non_neg_integer().
severity_to_int(debug) -> 0;
severity_to_int(verbose) -> 1;
severity_to_int(info) -> 2;
severity_to_int(warning) -> 3; 
severity_to_int(alert) ->  4.
