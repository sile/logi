%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
-module(logi_backend_stdout).

-behaviour(logi_backend).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([install/1, install/2]).
-export([uninstall/0, uninstall/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec install(logi_condition:spec()) -> ok.
install(ConditionSpec) ->
    install(logi:default_logger(), ConditionSpec).

-spec install(logi:logger(), logi_condition:spec()) -> ok.
install(Logger, ConditionSpec) ->
    logi:set_backend(Logger, {?MODULE, ?MODULE, []}, ConditionSpec).

-spec uninstall() -> ok.
uninstall() ->
    uninstall(logi:default_logger()).

-spec uninstall(logi:logger()) -> ok.
uninstall(Logger) ->
    logi:delete_backend(Logger, ?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec write(logi_backend:backend(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> any().
write(_Backend, Location, MsgInfo, Format, Args) ->
    io:format("~s [~s] ~p ~p ~s:~p [~s] ~s\n",
              [format_timestamp(logi_msg_info:get_timestamp(MsgInfo)),
               logi_msg_info:get_severity(MsgInfo),
               logi_location:get_node(Location),
               logi_location:get_process(Location),
               logi_location:get_module(Location),
               logi_location:get_line(Location),
               [[atom_to_list(K),"=",io_lib:format("~p", [V])] || {K, V} <- logi_msg_info:get_headers(MsgInfo)],
               io_lib:format(Format, Args)]).

-spec format_timestamp(erlang:timestamp()) -> iodata().
format_timestamp(Timestamp) ->
    {_, _, Micros} = Timestamp,
    Millis = Micros div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                  [Year, Month, Day, Hour, Minute, Second, Millis]).
