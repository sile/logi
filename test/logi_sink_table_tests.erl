%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_sink_table_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(CHANNEL, logi_test_log).
-define(NULL_SINK, logi_builtin_sink_null).

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
    Layout = logi_builtin_layout_simple:new(),
    {foreach, local,
     fun ()  -> logi_sink_table:new(?CHANNEL) end,
     fun (T) -> ok = logi_sink_table:delete(T) end,
     [
      {"Registers a sink",
       fun () ->
               ?assertEqual(ok, logi_sink_table:register(?CHANNEL, logi_sink:new(?NULL_SINK), undefined, Layout)),
               ?assertEqual([logi_builtin_sink_null], logi_sink_table:which_sinks(?CHANNEL))
       end},
      {"Deregisters a sink",
       fun () ->
               ok = logi_sink_table:register(?CHANNEL, logi_sink:new(hoge, ?NULL_SINK), undefined, Layout),
               ok = logi_sink_table:register(?CHANNEL, logi_sink:new(fuga, ?NULL_SINK), undefined, Layout),
               ?assertEqual(lists:sort([hoge, fuga]), lists:sort(logi_sink_table:which_sinks(?CHANNEL))),

               ?assertEqual(ok, logi_sink_table:deregister(?CHANNEL, logi_sink:new(hoge, ?NULL_SINK))),
               ?assertEqual([fuga], logi_sink_table:which_sinks(?CHANNEL)),

               ?assertEqual(ok, logi_sink_table:deregister(?CHANNEL, logi_sink:new(fuga, ?NULL_SINK))),
               ?assertEqual([], logi_sink_table:which_sinks(?CHANNEL))
       end}
     ]}.

select_test_() ->
    Layout = logi_builtin_layout_simple:new(),
    {foreach, local,
     fun ()  -> logi_sink_table:new(?CHANNEL) end,
     fun (T) -> ok = logi_sink_table:delete(T) end,
     [
      {"Selects sinks that meet the condition",
       fun () ->
               Sink = fun (Id, Condition) -> logi_sink:new(Id, ?NULL_SINK, Condition, Id) end,
               Sink1 = Sink(s1, debug),
               Sink2 = Sink(s2, {info, alert}),
               Sink3 = Sink(s3, [info]),
               Sink4 = Sink(s4, #{severity => info, application => stdlib}),
               Sink5 = Sink(s5, #{severity => info, module => lists}),

               lists:foreach(fun (S) -> logi_sink_table:register(?CHANNEL, S, undefined, Layout) end,
                             [Sink1, Sink2, Sink3, Sink4, Sink5]),

               Select =
                   fun (Severity, Application, Module) ->
                           lists:sort([Id || {_, Id, _} <- logi_sink_table:select(?CHANNEL, Severity, Application, Module)])
                   end,

               ?assertEqual([s1],                 Select(debug, stdlib, lists)),
               ?assertEqual([s1, s2, s3, s4, s5], Select(info, stdlib, lists)),
               ?assertEqual([s1, s2, s4],         Select(notice, stdlib, dict))
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
                                          Sinks = logi_sink_table:select(?CHANNEL, info, stdlib, lists),
                                          ?assert(Sinks =:= [] orelse length(Sinks) =:= 1)
                                  end,
                                  lists:seq(1, 1000))
                        end)) || _ <- lists:seq(1, 100)],
               SelectLoop =
                   fun Recur ([])   -> ok;
                       Recur (List) ->
                           receive
                               {'DOWN', Monitor, _, _, _} -> Recur(lists:delete(Monitor, List))
                           after 0 ->
                                   Sink = logi_sink:new(hoge, ?NULL_SINK),
                                   ok = logi_sink_table:register(?CHANNEL, Sink, undefined, Layout),
                                   logi_sink_table:deregister(?CHANNEL, Sink),
                                   Recur(List)
                           end
                   end,
               SelectLoop(Monitors)
       end}
     ]}.
