%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_app_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
start_and_stop_test_() ->
    [
     {"Starts `logi` application",
      fun () ->
              ?assertNot(lists:keymember(logi, 1, application:which_applications())),
              ?assertEqual(ok, application:start(logi)),
              ?assert(lists:keymember(logi, 1, application:which_applications())),
              ?assertEqual(ok, application:stop(logi)),
              ?assertNot(lists:keymember(logi, 1, application:which_applications()))
      end},
     {"The default channel is created automatically",
      fun () ->
              ok = application:start(logi),
              ?assertEqual([logi_channel:default_channel()], logi_channel:which_channels()),
              ok = application:stop(logi)
      end},
     {"If the default channel name conflicts with other existings process name, starting logi application will fail",
      fun () ->
              register(logi_channel:default_channel(), self()),
              ?assertMatch({error, _}, application:start(logi)),
              unregister(logi_channel:default_channel())
      end}
    ].
