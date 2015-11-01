%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_sink_fun_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
install_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Installs a `logi_builtin_sink_fun' sink",
       fun () ->
               WriteFun = fun (_, _, Format, Data) ->  io_lib:format(Format, Data) end,
               ?assertMatch({ok, _}, logi_builtin_sink_fun:install(info, WriteFun)),
               ?assertEqual([logi_builtin_sink_fun], logi_channel:which_sinks(logi_channel:default_channel()))
       end},
      {"Uninstalls a `logi_builtin_sink_fun` sink",
       fun () ->
               WriteFun = fun (_, _, Format, Data) ->  io_lib:format(Format, Data) end,
               {ok, _} = logi_builtin_sink_fun:install(info, WriteFun),
               [logi_builtin_sink_fun] = logi_channel:which_sinks(logi_channel:default_channel()),

               ?assertMatch({ok, _}, logi_builtin_sink_fun:uninstall()),
               ?assertEqual([], logi_channel:which_sinks(logi_channel:default_channel()))
       end},
      {"[ERORR] The second argument is not a `write_fun/0'",
       fun () ->
               ?assertError(badarg, logi_builtin_sink_fun:install(info, hoge)), % not a function
               ?assertError(badarg, logi_builtin_sink_fun:install(info, fun lists:map/2))  % not a `write_fun/0' function
       end}
     ]}.

write_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Writes a log message",
       fun () ->
               Issuer = self(),
               WriteFun = fun (_, _, Format, Data) -> Issuer ! {write, Format, Data} end,
               {ok, _} = logi_builtin_sink_fun:install(info, WriteFun),
               logi:info("hello world"),
               receive
                   {write, "hello world", []} -> ?assert(true)
               after 25 -> ?assert(timeout)
               end
       end}
     ]}.
