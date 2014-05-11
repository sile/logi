%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_msg_context).

-include("logi_internal.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([load_context/1,
         save_context/2,
         erase_context/1]).

-export([get_info/2, get_info/3,
         update_info/3, update_info/4,
         delete_info/2, delete_info/3]).

-export_type([context/0, info/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Callback API
%%------------------------------------------------------------------------------------------------------------------------
-callback is_instance(info() | term()) -> boolean().
-callback empty() -> info().
     
%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(PROCESS_DICTIONARY_TAG, '__LOGI_MSG_CONTEXT__').

-opaque context() :: [{logi:event_manager_ref(), info()}].

-type info() :: log_msg_header:header() | log_msg_metadata:metadata().

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-spec load_context(module()) -> context().
load_context(InfoModule) ->
    case get({?PROCESS_DICTIONARY_TAG, InfoModule}) of
        undefined -> [];
        Context   -> Context
    end.

-spec save_context(module(), context()) -> ok.
save_context(InfoModule, Context) ->
    ok = validate_context(InfoModule, Context),
    _ = put({?PROCESS_DICTIONARY_TAG, InfoModule}, Context),
    ok.

-spec erase_context(module()) -> ok.
erase_context(InfoModule) ->
    _ = erase({?PROCESS_DICTIONARY_TAG, InfoModule}),
    ok.

-spec get_info(logi:event_manager_ref(), module()) -> info().
get_info(ManagerRef, InfoModule) ->
    get_info(ManagerRef, InfoModule, load_context(InfoModule)).

-spec get_info(logi:event_manager_ref(), module(), context()) -> info().
get_info(ManagerRef, InfoModule, Context) ->
    case lists:keyfind(ManagerRef, 1, Context) of
        false     -> InfoModule:empty();
        {_, Info} -> Info
    end.

-spec update_info(logi:event_manager_ref(), UpdateFun, module()) -> context() when
      UpdateFun :: fun ((info()) -> info()).
update_info(ManagerRef, UpdateFun, InfoModule) ->
    Context = update_info(ManagerRef, UpdateFun, InfoModule, load_context(InfoModule)),
    ok = save_context(InfoModule, Context),
    Context.

-spec update_info(logi:event_manager_ref(), UpdateFun, module(), context()) -> context() when
      UpdateFun :: fun ((info()) -> info()).
update_info(ManagerRef, UpdateFun, InfoModule, Context0) ->
    {Info1, Context2} =
        case lists:keytake(ManagerRef, 1, Context0) of
            false                         -> {InfoModule:empty(), Context0};
            {value, {_, Info0}, Context1} -> {Info0, Context1}
        end,
    Info2 = UpdateFun(Info1),
    [{ManagerRef, Info2} | Context2].

-spec delete_info(logi:event_manager_ref(), module()) -> context().
delete_info(ManagerRef, InfoModule) ->
    Context = delete_info(ManagerRef, InfoModule, load_context(InfoModule)),
    ok = save_context(InfoModule, Context),
    Context.

-spec delete_info(logi:event_manager_ref(), module(), context()) -> context().
delete_info(ManagerRef, _InfoModule, Context) ->
    lists:keydelete(ManagerRef, 1, Context).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec validate_context(module(), context() | term()) -> ok.
validate_context(_InfoModule, []) ->
    ok;
validate_context(InfoModule, [{ManagerRef, Info} | Rest]) ->
    true = ?ASSERT_PRED_FUN(logi_event_manager, is_event_manager_ref, ManagerRef),
    true = ?ASSERT_PRED_FUN(InfoModule, is_instance, Info),
    validate_context(InfoModule, Rest);
validate_context(_InfoModule, [WrongEntry | _]) ->
    error({wrong_context_entry, WrongEntry});
validate_context(_InfoModule, WrongContext) ->
    error({wrong_context, WrongContext}).
