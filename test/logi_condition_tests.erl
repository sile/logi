%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_condition_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
condition_test_() ->
    [
     {"Valid Conditions",
      fun () ->
              IsValid = fun logi_condition:is_condition/1,
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
              IsValid = fun logi_condition:is_condition/1,
              ?assertNot(IsValid(undefined_severity)), % undefined severity
              ?assertNot(IsValid(#{application => "lists"})), % not an atom
              ?assertNot(IsValid(#{module => "lists"})), % not an atom
              ?assertNot(IsValid(#{severity => info, module => 'UNDEFINED_MOD'})) % undefined module
      end},
     {"Normalized conditions",
      fun () ->
              N = fun (Condition) ->
                          lists:sort(logi_condition:normalize(Condition))
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
