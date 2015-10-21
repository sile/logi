%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A simple built-in layout
%%
%% This module layouts a log message by the following format:
%% ```
%% {yyyy}-{MM}-{dd} {HH}:{mm}:{ss}.{SSS} [{SEVERITY}] {NODE} {PID} {MODULE}:{FUNCTION}:{LINE} [{HEADER(KEY=VALUE)}*] {MESSAGE}
%% '''
%%
%% == NOTE ==
%% This module is provided for debuging/testing purposes only.
%% The message format is not customizable.
%% And no overload protection exists (e.g. if log message is too large, the caller process may hang).
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > Layout = logi_builtin_layout_simple:new().
%% > logi_builtin_sink_io_device:install(info, [{layout, Layout}]).
%% > logi:info("hello world").
%% 2015-10-21 15:06:42.842 [info] nonode@nohost &lt;0.91.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
-module(logi_builtin_layout_simple).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a layout
-spec new() -> logi_layout:layout().
new() -> logi_layout:new(?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(logi_context:context(), io:format(), logi_layout:data(), undefined) -> iodata().
format(Context, Format, Data, _) ->
    Location = logi_context:get_location(Context),
    io_lib:format(
      "~s [~s] ~p ~p ~s:~s:~p [~s] " ++ re:replace(Format, "~p", "~1000000p", [global, {return, list}]) ++ "\n",
      [
       format_timestamp(logi_context:get_timestamp(Context)),
       logi_context:get_severity(Context),
       node(logi_location:get_process(Location)),
       logi_location:get_process(Location),
       logi_location:get_module(Location),
       logi_location:get_function(Location),
       logi_location:get_line(Location),
       format_headers(logi_context:get_headers(Context)) |
       Data
      ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec format_timestamp(erlang:timestamp()) -> iodata().
format_timestamp(Timestamp) ->
    {_, _, Micros} = Timestamp,
    Millis = Micros div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                  [Year, Month, Day, Hour, Minute, Second, Millis]).

-spec format_headers(logi:headers()) -> iodata().
format_headers(Headers) ->
    string:join([[atom_to_list(K),"=",to_string(V)] || {K, V} <- maps:to_list(Headers)], ",").

-spec to_string(term()) -> iodata().
to_string(V) when is_binary(V)    -> binary_to_list(V);
to_string(V) when is_atom(V)      -> atom_to_list(V);
to_string(V) when is_integer(V)   -> integer_to_list(V);
to_string(V) when is_float(V)     -> float_to_list(V);
to_string(V) when is_function(V)  -> erlang:fun_to_list(V);
to_string(V) when is_pid(V)       -> erlang:pid_to_list(V);
to_string(V) when is_port(V)      -> erlang:port_to_list(V);
to_string(V) when is_reference(V) -> erlang:ref_to_list(V);
to_string(V) when is_list(V)      ->
    IsNonNegInteger = fun (C) -> is_integer(C) andalso C >= 0 end,
    case lists:all(IsNonNegInteger, V) of
        true  -> V;
        false -> io_lib:format("~1000000p", [V])
    end;
to_string(V) ->
    io_lib:format("~1000000p", [V]).
