%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_client).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([log/5]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%%-spec log(logi:severity(), atom(), module(), pos_integer(), iodata(), [term()], #logi_log_option{}) -> ok.
log(Severity, Format, Args, Context, Options) ->
    MetaData = make_full_metadata(Context, Options),
    case logi_backend_manager:select_backends(Context#logi_log_context.manager, Severity, MetaData) of
        []       -> ok;
        Backends ->
            FrequencyId = {Context#logi_log_context.module, Context#logi_log_context.line},
            case logi_frequency_control:is_logging_turn(Options#logi_log_option.frequency, FrequencyId) of
                false                -> ok;
                {true, OmittedCount} ->
                    FormatOptions =
                        #logi_format_option{
                           severity      = Severity,
                           metadata      = MetaData,
                           header        = make_full_header(Context, MetaData, Options),
                           omitted_count = OmittedCount
                          },
                    lists:foreach(
                      fun (Backend) ->
                              %% TODO: try-catch
                              Module = logi_backend:get_module(Backend),
                              Message = Module:format(Backend, Format, Args, FormatOptions),
                              Module:write(Backend, Message)
                      end,
                      Backends)
            end
    end.
        
%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec make_full_header(#logi_log_context{}, [logi:metadata_entry()], #logi_log_option{}) -> [logi:header_entry()].
make_full_header(Context, MetaData, Options) ->
    DefaultEntries = logi:get_header([{manager, Context#logi_log_context.manager}, {scope, MetaData}]),
    lists:ukeymerge(1, lists:ukeysort(1, Options#logi_log_option.header), DefaultEntries).

-spec make_full_metadata(#logi_log_context{}, #logi_log_option{}) -> [logi:metadata_entry()].
make_full_metadata(Context, Options) ->
    #logi_log_context{manager = Manager, application = Application, module = Module, line = Line} = Context,
    DefaultEntries0 = [{application, Application},
                       {line, Line},
                       {module, Module},
                       {node, node()},
                       {pid, self()}],
    StoredEntries   = logi:get_metadata([{manager, Manager}]),
    DefaultEntries1 = lists:ukeymerge(1, StoredEntries, DefaultEntries0),
    lists:ukeymerge(1, lists:ukeysort(1, Options#logi_log_option.metadata), DefaultEntries1).
