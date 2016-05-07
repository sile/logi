%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
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
              Sink = logi_builtin_sink_fun:new(id, WriteFun),
              ?assert(logi_sink:is_sink(Sink))
      end},
     {"[ERORR] The argument is not a `write_fun/0'",
      fun () ->
              ?assertError(badarg, logi_builtin_sink_fun:new(id, hoge)), % not a function
              ?assertError(badarg, logi_builtin_sink_fun:new(id, fun lists:map/2))  % not a `write_fun/0' function
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
               {ok, _} = logi_channel:install_sink(logi_builtin_sink_fun:new(test, WriteFun), info),
               logi:info("hello world"),
               receive
                   {write, "hello world", []} -> ?assert(true)
               after 25 -> ?assert(timeout)
               end
       end}
     ]}.

get_writee_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"The writee is undefined",
       fun () ->
               WriteFun = fun (_, _, _) -> [] end,
               {ok, _} = logi_channel:install_sink(logi_builtin_sink_fun:new(test, WriteFun), info),
               {ok, #{writer := Writer}} = logi_channel:find_sink(test),
               ?assertEqual(undefined, logi_sink_writer:get_writee(Writer))
       end}
     ]}.
