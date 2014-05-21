%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_client).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([log/7]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec log(logi:backend_manager(), logi:severity(), logi:location(),
          io:format(), [term()], logi:log_options(), logi:context()) -> logi:context().
log(Manager, Severity, Location, Format, Args, Options, Context) ->
    MetaData = logi_context:get_full_metadata(logi_util_assoc:fetch(metadata, Options, []), Context),
    case logi_backend_manager:select_backends(Manager, Severity, Location, MetaData) of
        []       -> Context;
        Backends ->
            case logi_context:is_output_allowed(logi_util_assoc:fetch(frequency, Options, always), Location, Context) of
                {false, Context2}                 -> Context2;
                {{true, OmittedCount},  Context2} ->
                    Headers = logi_context:get_full_headers(logi_util_assoc:fetch(headers, Options, []), Context2),
                    MsgInfo = logi_msg_info:make(Severity, os:timestamp(), Headers, MetaData, OmittedCount),
                    ok = lists:foreach(
                           fun (Backend) ->
                                   Module = logi_backend:get_module(Backend),
                                   case format_message(Module, Backend, Location, Format, Args, MsgInfo) of
                                       error         -> ok;
                                       {ok, Message} -> write_message(Module, Backend, Message)
                                   end
                           end,
                           Backends),
                    Context2
            end
    end.
        
%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec format_message(module(), logi:backend(), logi:location(), io:format(), [term()], logi:msg_info()) -> {ok, iodata()} | error.
format_message(Module, Backend, Location, Format, Args, MsgInfo) ->
    try 
        {ok, Module:format(Backend, Location ,Format, Args, MsgInfo)}
    catch
        Class:Reason ->
            ok = error_logger:error_msg(
                   logi_io_lib:format_error(Class, Reason, erlang:get_stacktrace(),
                                            "~p:format/4 error: args=~p", [Module, [Backend, Location, Format, Args, MsgInfo]])),
            error
    end.

-spec write_message(module(), logi:backend(), iodata()) -> any().
write_message(Module, Backend, Message) ->
    try
        Module:write(Backend, Message)
    catch
        Class:Reason ->
            error_logger:error_msg(
              logi_io_lib:format_error(Class, Reason, erlang:get_stacktrace(),
                                       "~p:write/2 error: args=~p", [Module, [Backend, Message]]))
    end.
