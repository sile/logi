%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_builtin_sink_fun_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creats a new sink instance",
      fun () ->
              WriteFun = fun (_, Format, Data) ->  io_lib:format(Format, Data) end,
              Sink = logi_builtin_sink_fun:new(WriteFun),
              ?assert(logi_sink:is_spec(Sink))
      end},
     {"[ERORR] The argument is not a `write_fun/0'",
      fun () ->
              ?assertError(badarg, logi_builtin_sink_fun:new(hoge)), % not a function
              ?assertError(badarg, logi_builtin_sink_fun:new(fun lists:map/2))  % not a `write_fun/0' function
      end}
    ].

write_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Writes a log message",
       fun () ->
               Issuer = self(),
               WriteFun = fun (_, Format, Data) -> Issuer ! {write, Format, Data} end,
               {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun)),
               logi:info("hello world"),
               receive
                   {write, "hello world", []} -> ?assert(true)
               after 25 -> ?assert(timeout)
               end
       end}
     ]}.
