%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_sink_io_device_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new sink instance",
      fun () ->
              Sink = logi_builtin_sink_io_device:new(),
              ?assert(logi_sink:is_spec(Sink))
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
               Sink = logi_builtin_sink_io_device:new([{io_device, Fd}, {layout, Layout}]),
               {ok, _} = logi_channel:install_sink(info, Sink, [{layout, Layout}]),
               logi:info("hello world"),
               ok = file:close(Fd),
               ?assertEqual({ok, <<"hello world">>}, file:read_file("test.log")),
               ok = file:delete("test.log")
       end}
     ]}.
