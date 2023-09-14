%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_location_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new location object",
      fun () ->
              L = logi_location:new(lists, map, 0),
              ?assert(logi_location:is_location(L))
      end},
     {"Converts from/to a map",
      fun () ->
              L = logi_location:new(lists, map, 0),
              ?assertEqual(L, logi_location:from_map(logi_location:to_map(L)))
      end},
     {"Converts from an empty map",
      fun () ->
              L = logi_location:new(self(), undefined, undefined, undefined, 0),
              ?assertEqual(L, logi_location:from_map(#{}))
      end}
    ].

get_test_() ->
    L = logi_location:new(self(), stdlib, lists, map, 10),
    [
     ?_assertEqual(self(), logi_location:get_process(L)),
     ?_assertEqual(stdlib, logi_location:get_application(L)),
     ?_assertEqual(lists,  logi_location:get_module(L)),
     ?_assertEqual(map,    logi_location:get_function(L)),
     ?_assertEqual(10,     logi_location:get_line(L))
    ].

guess_test_() ->
    {setup,
     fun ()  -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {"Guesses the location",
       fun () ->
               Pid = self(),
               ?assertMatch(
                  #{
                     process     := Pid,
                     application := undefined,
                     module      := ?MODULE,
                     line        := 54
                   },
                  logi_location:to_map(logi_location:guess_location()))
       end},
      {"Guesses the application",
       fun () ->
               ?assertEqual(stdlib, logi_location:guess_application(lists)),
               ?assertEqual(undefined, logi_location:guess_application('UNDEFINED_MODULE'))
       end}
     ]}.
