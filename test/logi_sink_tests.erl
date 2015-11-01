%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_sink_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(NULL_SINK, logi_builtin_sink_null).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a sink",
      fun () ->
              S0 = logi_sink:new(?NULL_SINK),
              S1 = logi_sink:new(?NULL_SINK, debug),
              S2 = logi_sink:new(?NULL_SINK, debug, undefined),
              ?assert(logi_sink:is_sink(S0)),
              ?assertMatch(S0 = S1, S2)
      end},
     {"[ERROR] a module that does not implement `sink' behaviour was passed",
      fun () ->
              ?assertError(badarg, logi_sink:new(lists))
      end},
     {"Converts to/from a map",
      fun () ->
              %% from_map/1
              S = logi_sink:new(?NULL_SINK),
              ?assertEqual(S, logi_sink:from_map(#{module => ?NULL_SINK})),

              %% to_map/1
              M = #{module => ?NULL_SINK, condition => debug, extra_data => undefined},
              ?assertEqual(M, logi_sink:to_map(S)),

              %% S = from_map(to_map(S))
              ?assertEqual(S, logi_sink:from_map(logi_sink:to_map(S)))
      end},
     {"[ERROR] `module' field is mandatory",
      fun () ->
              ?assertError(badarg, logi_sink:from_map(#{}))
      end}
    ].

get_test_() ->
    [
     {"Gets the information from a sink",
      fun () ->
              S = logi_sink:new(?NULL_SINK, info, "EXTRA"),
              ?assertEqual(?NULL_SINK, logi_sink:get_module(S)),
              ?assertEqual(info,       logi_sink:get_condition(S)),
              ?assertEqual("EXTRA",    logi_sink:get_extra_data(S))
      end}
    ].

condition_test_() ->
    [
     {"Valid Conditions",
      fun () ->
              IsValid = fun logi_sink:is_condition/1,
              ?assert(IsValid(info)),
              ?assert(IsValid({info, alert})),
              ?assert(IsValid({alert, info})),
              ?assert(IsValid([debug, info])),
              ?assert(IsValid(#{})),
              ?assert(IsValid(#{application => kernel})),
              ?assert(IsValid(#{severity => info, application => kernel})),
              ?assert(IsValid(#{severity => info, application => [kernel, stdlib]})),
              ?assert(IsValid(#{severity => info, module => lists})),
              ?assert(IsValid(#{severity => info, module => [lists, dict]})),
              ?assert(IsValid(#{severity => [info], application => kernel, module => [lists]}))
      end},
     {"Invalid Conditions",
      fun () ->
              IsValid = fun logi_sink:is_condition/1,
              ?assertNot(IsValid(undefined_severity)), % undefined severity
              ?assertNot(IsValid(#{application => "lists"})), % not an atom
              ?assertNot(IsValid(#{module => "lists"})), % not an atom
              ?assertNot(IsValid(#{severity => info, module => 'UNDEFINED_MOD'})) % undefined module
      end},
     {"Normalized conditions",
      fun () ->
              N = fun (Condition) ->
                          Sink = logi_sink:new(?NULL_SINK, Condition),
                          lists:sort(logi_sink:get_normalized_condition(Sink))
                  end,

              ?assertEqual([alert, critical, emergency], N(critical)),
              ?assertEqual([info, notice, warning],      N({info, warning})),
              ?assertEqual([],                           N({warning, info})),
              ?assertEqual([],                           N([])),
              ?assertEqual([alert, debug, info],         N([debug, info, alert])),
              ?assertEqual([],                           N(#{})),
              ?assertEqual([],                           N(#{severity => info})),
              ?assertEqual(lists:sort([{S, kernel} || S <- logi:severities()]),
                           N(#{
                                application => kernel
                              })),
              ?assertEqual([{alert, kernel}, {emergency, kernel}],
                           N(#{
                                severity => alert, application => kernel
                              })),
              ?assertEqual([{info, kernel}, {info, stdlib}],
                           N(#{
                                severity => {info, info}, application => [kernel, stdlib]
                              })),
              ?assertEqual([{info, stdlib, lists}],
                           N(#{
                                severity => [info], module => lists
                              })),
              ?assertEqual([{info, stdlib, dict}, {info, stdlib, lists}],
                           N(#{
                                severity => [info], module => [lists, dict]
                              })),
              ?assertEqual([{info, kernel}, {info, stdlib, lists}],
                           N(#{
                                severity => [info], application => kernel, module => lists
                              }))
      end}
    ].
