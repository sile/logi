%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_sink_writer_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a writer",
      fun () ->
              Writer = logi_sink_writer:new(logi_builtin_sink_fun, fun (_, _, _) -> [] end),
              ?assert(logi_sink_writer:is_writer(Writer))
      end},
     {"Not a writer",
      fun () ->
              ?assertNot(logi_sink_writer:is_writer(not_a_writer))
      end}
    ].

get_test_() ->
    Fun = fun (_, _, _) -> [] end,
    Writer = logi_sink_writer:new(logi_builtin_sink_fun, Fun),
    [
     ?_assertEqual(logi_builtin_sink_fun, logi_sink_writer:get_module(Writer)),
     ?_assertEqual(Fun, logi_sink_writer:get_state(Writer))
    ].
