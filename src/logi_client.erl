%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_client).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([log/7]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec log(logi:severity(), atom(), module(), pos_integer(), iodata(), [term()], #logi_log_option{}) -> ok.
log(Severity, Application, Module, Line, Format, Args, Options) ->
    MetaData = make_full_metadata(Application, Module, Line, Options),
    case logi_event_manager:get_handlers(Options#logi_log_option.manager, MetaData) of
        []       -> ok;
        Handlers ->
            case frequency_check(Module, Line, Options) of % XXX: name
                false             -> ok;
                {true, OmitCount} ->
                    FormatOptions =
                        #logi_format_option{
                           severity      = Severity,
                           metadata      = MetaData,
                           header        = make_full_header(MetaData, Options),
                           omitted_count = OmitCount
                          },
                    lists:foreach(
                      fun ({Handler, HandlerArg}) ->
                              try
                                  Msg = Handler:format(HandlerArg, Format, Args, FormatOptions),
                                  Handler:write(HandlerArg, Msg)
                              catch
                                  _ExClass:_ExReason ->
                                      %% TODO: log
                                      ok
                              end
                      end,
                      Handlers)
            end
    end.
        
%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec make_full_header([logi:metadata_entry()], #logi_log_option{}) -> [logi:header_entry()].
make_full_header(ScopeList, Options) ->
    DefaultEntries = logi:get_header([{manager, Options#logi_log_option.manager}, {scope, ScopeList}]),
    lists:ukeymerge(1, lists:ukeysort(1, Options#logi_log_option.header), DefaultEntries).

-spec make_full_metadata(atom(), module(), pos_integer(), #logi_log_option{}) -> [logi:metadata_entry()].
make_full_metadata(Application, Module, Line, Options) ->
    DefaultEntries0 = [{application, Application},
                       {line, Line},
                       {module, Module},
                       {node, node()},
                       {pid, self()}],
    StoredEntries   = logi:get_metadata([{manager, Options#logi_log_option.manager}]),
    DefaultEntries1 = lists:ukeymerge(1, StoredEntries, DefaultEntries0),
    lists:ukeymerge(1, lists:ukeysort(1, Options#logi_log_option.metadata), DefaultEntries1).

%% XXX: name
-spec frequency_check(module(), pos_integer(), #logi_log_option{}) -> {true, non_neg_integer()} | false.
frequency_check(Module, Line, #logi_log_option{frequency = Policy, frequency_id = Id}) ->
    frequency_check(Policy, Module, Line, Id).

-spec frequency_check(logi:frequency_policy(), module(), pos_integer(), term()) -> {ok, non_neg_integer()} | false.
frequency_check(always, _, _, _) ->
    {true, 0};
frequency_check(once, Module, Line, Id) ->
    case logi_frequency_manager:get_frequency_count(Id, Module, Line) of
        0 -> {true, 0};
        _ -> false
    end;
frequency_check({interval_count, Interval}, Module, Line, Id) ->
    Count = logi_frequency_manager:get_frequency_count(Id, Module, Line),
    case Count rem Interval of
        0 ->
            ok = logi_frequency_manager:increment_omitted_count(Id, Module, Line),
            {true, case Count of 0 -> 0; _ -> Interval - 1 end};
        _ -> false
    end;
frequency_check({interval_time, Interval}, Module, Line, Id) ->
    Prevous = logi_frequency_manager:get_previous_time(Id, Module, Line),
    Now = os:timestamp(),
    case timer:now_diff(Now, Prevous) div 1000 < Interval of
        true  ->
            ok = logi_frequency_manager:increment_omitted_count(Id, Module, Line),
            false;
        false ->
            ok = logi_frequency_manager:set_logged_timestamp(Id, Module, Line, Now),
            OmittedCount = logi_frequency_manager:reset_ommited_count(Id, Module, Line),
            {true, OmittedCount}
    end.
