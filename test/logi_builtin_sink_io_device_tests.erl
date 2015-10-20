%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_sink_io_device_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
install_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Installs a `logi_builtin_sink_io_device' sink",
       fun () ->
               ?assertMatch({ok, _}, logi_builtin_sink_io_device:install(info)),
               ?assertEqual([logi_builtin_sink_io_device], logi_channel:which_sinks(logi_channel:default_channel()))
       end},
      {"Uninstalls a `logi_builtin_sink_io_device` sink",
       fun () ->
               {ok, _} = logi_builtin_sink_io_device:install(info),
               [logi_builtin_sink_io_device] = logi_channel:which_sinks(logi_channel:default_channel()),

               ?assertMatch({ok, _}, logi_builtin_sink_io_device:uninstall()),
               ?assertEqual([], logi_channel:which_sinks(logi_channel:default_channel()))
       end}
     ]}.
