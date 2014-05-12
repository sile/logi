%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_client).

-include("logi.hrl"). % TODO: logi.hrl => logi_internal.hrl

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([log/6]).

-export_type([log_option/0, frequency/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type log_option() :: {manager, logi:event_manager_ref()}
                    | {header, [logi:header_entry()]}
                    | {metadata, [logi:metadata_entry()]}
                    | {frequency, frequency()}
                    | {owner, pid()}.

-type frequency()  :: once
                    | {interval_count, non_neg_integer()}
                    | {interval_time, timeout()}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec log(logi:severity(), module(), pos_integer(), iolist(), [term()], [log_option()]) -> ok.
log(Severity, Module, Line, Format, Args, Options) ->
    ManagerRef = logi_util_assoc:fetch(manager, Options, ?LOGI_DEFAULT_EVENT_MANAGER),
    MetaData = make_full_metadata(ManagerRef, Module, Line, logi_util_assoc:fetch(metadata, Options, [])),
    case logi_event_manager:get_handlers(ManagerRef, MetaData) of
        []       -> ok;
        Handlers ->
            case frequency_check(Module, Line, Options) of % XXX: name
                false             -> ok;
                {true, OmitCount} ->
                    MetaData2 = [{omit_count, OmitCount} | MetaData], % XXX:
                    Header = make_full_header(ManagerRef, MetaData2, logi_util_assoc:fetch(header, Options, [])),
                    lists:foreach(
                      fun (Handler) ->
                              try
                                  Msg = Handler:format(MetaData2, Header, Format, Args),
                                  Handler:write(Msg)
                              catch
                                  _ExClass:_ExReason ->
                                      %% TODO: log
                              end
                      end,
                      Handlers)
            end
    end.
        
%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec make_full_header(logi:event_manager_ref(), [logi:metadata_entry()], [logi:header_entry()]) -> [logi:header_entry()].
make_full_header(ManagerRef, ScopeList, PriorEntries) ->
    DefaultEntries = logi:get_header([{manager, ManagerRef}, {scope, ScopeList}]),
    lists:ukeymerge(1, lists:ukeysort(1, PriorEntries), DefaultEntries).

-spec make_full_metadata(logi:event_manager_ref(), module(), pos_integer(), [logi:metadata_entry()]) -> [logi:metadata_entry()].
make_full_metadata(ManagerRef, Module, Line, PriorEntries) ->
    DefaultEntries0 = [{line, Line},
                       {module, Module},
                       %% TODO: {application, application:get_application(Module)}
                       {node, node()},
                       {pid, self()}],
    StoredEntries   = logi:get_metadata([{manager, ManagerRef}]),
    DefaultEntries1 = lists:ukeymerge(1, StoredEntries, DefaultEntries0),
    lists:ukeymerge(1, lists:ukeysort(1, PriorEntries), DefaultEntries1).

-spec frequency_check(module(), pos_integer(), [log_option()]) -> {true, non_neg_integer()} | false.
frequency_check(Module, Line, Options) ->
    case logi_util_assoc:fetch(frequency, Options, undefined) of
        undefined       -> {true, 0};
        FrequencyPolicy ->
            Owner = logi_util_assoc:fetch(owner, Options, self()),
            frequency_check(Module, Line, Owner, FrequencyPolicy)
    end.

-spec frequency_check(module(), pos_integer(), pid(), frequency()) -> {true, non_neg_integer()} | false.
frequency_check(Module, Line, Owner, once) ->
    todo;
frequency_check(Module, Line, Owner, {interval_count, Count}) ->
    todo;
frequency_check(Module, Line, Owner, {interval_time, Time}) ->
    todo.

