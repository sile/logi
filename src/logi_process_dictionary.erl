%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_process_dictionary).

-compile(inline).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([get_header/0, get_header/1,
         set_header/1, set_header/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(TAG, '__LOGI__').

-define(HEADER_KEY, {?TAG, header}).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-spec get_header() -> logi_client:whole_header_state().
get_header() ->
    case get(?HEADER_KEY) of
        undefined -> [];
        State     -> State
    end.

-spec get_header(logi:event_manager_ref()) -> {logi_client:header_state(), logi_client:whole_header_state()}.
get_header(ManagerRef) ->
    WholeHeaderState = get_header(),
    case lists:keytake(ManagerRef, 1, WholeHeaderState) of
        false                                        -> {[], WholeHeaderState};
        {value, {_, HeaderState}, WholeHeaderState2} -> {HeaderState, WholeHeaderState2}
    end.

-spec set_header(logi_client:whole_header_state()) -> ok.
set_header(State) ->
    _ = put(?HEADER_KEY, State),
    ok.

-spec set_header(logi:event_manager_ref(), logi_client:header_state(), logi_client:whole_header_state()) -> ok.
set_header(ManagerRef, HeaderState, WholeHeaderState) ->
    set_header([{ManagerRef, HeaderState} | WholeHeaderState]).

