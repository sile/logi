%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_builtin_sink_composite_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new sink",
      fun () ->
              Sink = logi_builtin_sink_composite:new(foo, [logi_builtin_sink_io_device:new(bar)]),
              ?assert(logi_sink:is_sink(Sink))
      end},
     {"Empty children is forbidden",
      fun () ->
              ?assertError(badarg, logi_builtin_sink_composite:new(foo, []))
      end}
    ].

composite_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Switches the active child (depth=1)",
       fun () ->
               Client = self(),

               Child1 = logi_builtin_sink_fun:new(bar, fun (_, _, _) -> Client ! {hello, bar} end),
               Child2 = logi_builtin_sink_fun:new(baz, fun (_, _, _) -> Client ! {hello, baz} end),
               Sink = logi_builtin_sink_composite:new(foo, [Child1, Child2]),
               {ok, _} = logi_channel:install_sink(Sink, info),

               %% Gets children
               SinkPid = logi_channel:whereis_sink_proc([foo]),
               ?assertEqual([Child1, Child2], logi_builtin_sink_composite:get_children(SinkPid)),

               %% Writes a message via first child
               logi:info("hello"),
               receive
                   {hello, Id0} -> ?assertEqual(bar, Id0)
               after 10 -> ?assert(timeout)
               end,

               %% Writes a message via second child
               logi_builtin_sink_composite:set_active_writer(SinkPid, 2),
               timer:sleep(10),
               logi:info("hello"),
               receive
                   {hello, Id1} -> ?assertEqual(baz, Id1)
               after 10 -> ?assert(timeout)
               end,

               %% Unsets the active child
               logi_builtin_sink_composite:unset_active_writer(SinkPid),
               timer:sleep(10),
               logi:info("hello"),
               receive
                   _ -> ?assert(unexpected_message)
               after 10 -> ?assert(true)
               end
       end},
      {"Switches the active child (depth=2)",
       fun () ->
               Client = self(),

               Child1_a = logi_builtin_sink_fun:new(bar_a, fun (_, _, _) -> Client ! {hello, bar_a} end),
               Child1_b = logi_builtin_sink_fun:new(bar_b, fun (_, _, _) -> Client ! {hello, bar_b} end),
               Child2_a = logi_builtin_sink_fun:new(baz_a, fun (_, _, _) -> Client ! {hello, baz_a} end),
               Child2_b = logi_builtin_sink_fun:new(baz_b, fun (_, _, _) -> Client ! {hello, baz_b} end),
               Sink = logi_builtin_sink_composite:new(
                        foo,
                        [
                         logi_builtin_sink_composite:new(bar, [Child1_a, Child1_b]),
                         logi_builtin_sink_composite:new(baz, [Child2_a, Child2_b])
                        ]),
               {ok, _} = logi_channel:install_sink(Sink, info),

               %% Writes a message via `bar_a' child (default)
               logi:info("hello"),
               receive
                   {hello, Id0} -> ?assertEqual(bar_a, Id0)
               after 10 -> ?assert(timeout)
               end,

               %% Writes a message via `bar_b' child
               BarPid = logi_channel:whereis_sink_proc([foo, bar]),
               logi_builtin_sink_composite:set_active_writer(BarPid, 2),
               timer:sleep(10),
               logi:info("hello"),
               receive
                   {hello, Id1} -> ?assertEqual(bar_b, Id1)
               after 10 -> ?assert(timeout)
               end,

               %% Writes a message via `baz_a' child
               FooPid = logi_channel:whereis_sink_proc([foo]),
               logi_builtin_sink_composite:set_active_writer(FooPid, 2),
               timer:sleep(10),
               logi:info("hello"),
               receive
                   {hello, Id2} -> ?assertEqual(baz_a, Id2)
               after 10 -> ?assert(timeout)
               end
       end},
      {"Start failure",
       fun () ->
               ValidChild0 = logi_builtin_sink_io_device:new(bar),
               InvalidChild = logi_sink:new(#{id => baz, start => {invalid_module, invalid_start, []}}),
               ValidChild1 = logi_builtin_sink_io_device:new(qux),
               Sink = logi_builtin_sink_composite:new(foo, [ValidChild0, InvalidChild, ValidChild1]),
               ?assertMatch({error, {cannot_start, _}}, logi_channel:install_sink(Sink, info))
       end}
     ]}.
