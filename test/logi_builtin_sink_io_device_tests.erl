%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_builtin_sink_io_device_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new sink instance",
      fun () ->
              Sink = logi_builtin_sink_io_device:new(id),
              ?assert(logi_sink:is_sink(Sink))
      end}
    ].

write_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Writes a log message",
       fun () ->
               {ok, Fd} = file:open("test.log", [write]),
               Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format(Format, Data) end),
               Sink = logi_builtin_sink_io_device:new(id, [{io_device, Fd}, {layout, Layout}]),
               {ok, _} = logi_channel:install_sink(Sink, info),
               logi:info("hello world"),
               ok = file:close(Fd),
               ?assertEqual({ok, <<"hello world">>}, file:read_file("test.log")),
               ok = file:delete("test.log")
       end}
     ]}.

get_writee_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Gets the writee process",
       fun () ->
               Sink = logi_builtin_sink_io_device:new(id, [{io_device, standard_error}]),
               {ok, _} = logi_channel:install_sink(Sink, info),
               {ok, #{writer := Writer}} = logi_channel:find_sink(id),
               ?assertEqual(whereis(standard_error), logi_sink_writer:get_writee(Writer))
       end}
     ]}.
