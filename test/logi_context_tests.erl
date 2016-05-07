%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_context_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new context object",
      fun () ->
              C = logi_context:new(logi_channel:default_channel(), os:timestamp(), info,
                                   logi_location:guess_location(), #{}, #{}),
              ?assert(logi_context:is_context(C))
      end},
     {"Converts from/to a map",
      fun () ->
              C = logi_context:new(logi_channel:default_channel(), os:timestamp(), info,
                                   logi_location:guess_location(), #{}, #{}),
              ?assertEqual(C, logi_context:from_map(logi_context:to_map(C)))
      end},
     {"[ERROR] Converts from an empty map",
      fun () ->
              ?assertError({badkey, _}, logi_context:from_map(#{}))
      end}
    ].

get_test_() ->
    Channel = logi_channel:default_channel(),
    Timestamp = os:timestamp(),
    Location = logi_location:guess_location(),
    C = logi_context:new(Channel, Timestamp, info, Location, #{}, #{}),
    [
     ?_assertEqual(Channel,   logi_context:get_channel(C)),
     ?_assertEqual(Timestamp, logi_context:get_timestamp(C)),
     ?_assertEqual(info,      logi_context:get_severity(C)),
     ?_assertEqual(Location,  logi_context:get_location(C)),
     ?_assertEqual(#{},       logi_context:get_headers(C)),
     ?_assertEqual(#{},       logi_context:get_metadata(C))
    ].
