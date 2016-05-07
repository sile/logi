%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_sink_table_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(CHANNEL, logi_test_log).
-define(WRITER(), ?WRITER(fun (_, _, _) -> [] end)).
-define(WRITER(Fun), logi_sink_writer:new(logi_builtin_sink_fun, Fun)).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates and delets a table",
      fun () ->
              Table = logi_sink_table:new(?CHANNEL),
              ?assertEqual(?CHANNEL, Table),
              ?assertEqual(ok, logi_sink_table:delete(Table))
      end},
     {"[ERROR] table name conflict",
      fun () ->
              Table = logi_sink_table:new(?CHANNEL),
              ?assertError(badarg, logi_sink_table:new(?CHANNEL)),
              ok = logi_sink_table:delete(Table)
      end}
    ].

register_test_() ->
    {foreach, local,
     fun ()  -> logi_sink_table:new(?CHANNEL) end,
     fun (T) -> ok = logi_sink_table:delete(T) end,
     [
      {"Registers a sink writer",
       fun () ->
               ?assertEqual(ok, logi_sink_table:register(?CHANNEL, foo, ?WRITER(), debug, [])),
               ?assertEqual([foo], logi_sink_table:which_sinks(?CHANNEL))
       end},
      {"Deregisters a sink writer",
       fun () ->
               ok = logi_sink_table:register(?CHANNEL, foo, ?WRITER(), debug, []),
               ok = logi_sink_table:register(?CHANNEL, bar, ?WRITER(), debug, []),
               ?assertEqual(lists:sort([foo, bar]), lists:sort(logi_sink_table:which_sinks(?CHANNEL))),

               ?assertEqual(ok, logi_sink_table:deregister(?CHANNEL, foo, debug)),
               ?assertEqual([bar], logi_sink_table:which_sinks(?CHANNEL)),

               ?assertEqual(ok, logi_sink_table:deregister(?CHANNEL, bar, debug)),
               ?assertEqual([], logi_sink_table:which_sinks(?CHANNEL))
       end}
     ]}.

select_test_() ->
    {foreach, local,
     fun ()  -> logi_sink_table:new(?CHANNEL) end,
     fun (T) -> ok = logi_sink_table:delete(T) end,
     [
      {"Selects sink writers that meet the condition",
       fun () ->
               Writer =
                   fun (Id, Condition) ->
                           Fun = fun (_, _, _) -> atom_to_list(Id) end,
                           {{Id, Condition, ?WRITER(Fun)}, Fun}
                   end,
               {Writer1, V1} = Writer(w1, debug),
               {Writer2, V2} = Writer(w2, {info, alert}),
               {Writer3, V3} = Writer(w3, [info]),
               {Writer4, V4} = Writer(w4, #{severity => info, application => stdlib}),
               {Writer5, V5} = Writer(w5, #{severity => info, module => lists}),

               lists:foreach(fun ({Id, C, S}) -> logi_sink_table:register(?CHANNEL, Id, S, C, []) end,
                             [Writer1, Writer2, Writer3, Writer4, Writer5]),

               Select =
                   fun (Severity, Application, Module) ->
                           lists:sort(
                             [logi_sink_writer:get_state(W) ||
                                 W <- logi_sink_table:select(?CHANNEL, Severity, Application, Module)])
                   end,

               ?assertEqual([V1],                 Select(debug, stdlib, lists)),
               ?assertEqual([V1, V2, V3, V4, V5], Select(info, stdlib, lists)),
               ?assertEqual([V1, V2, V4],         Select(notice, stdlib, dict))
       end},
      {"If an unknown table is specified, `select/4` will return an empty list",
       fun () ->
               ?assertEqual([], logi_sink_table:select('UNKNOWN_TABLE', debug, select, lists))
       end},
      {"Under high contention",
       fun () ->
               Monitors =
                   [element(
                      2,
                      spawn_monitor(
                        fun () ->
                                lists:foreach(
                                  fun (_) ->
                                          timer:sleep(1),
                                          Writers = logi_sink_table:select(?CHANNEL, info, stdlib, lists),
                                          ?assert(Writers =:= [] orelse length(Writers) =:= 1)
                                  end,
                                  lists:seq(1, 1000))
                        end)) || _ <- lists:seq(1, 100)],
               SelectLoop =
                   fun Recur ([])   -> ok;
                       Recur (List) ->
                           receive
                               {'DOWN', Monitor, _, _, _} -> Recur(lists:delete(Monitor, List))
                           after 0 ->
                                   Writer = ?WRITER(),
                                   ok = logi_sink_table:register(?CHANNEL, foo, Writer, debug, []),
                                   logi_sink_table:deregister(?CHANNEL, foo, debug),
                                   Recur(List)
                           end
                   end,
               SelectLoop(Monitors)
       end}
     ]}.
