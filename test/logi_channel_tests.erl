%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_channel_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
channel_test_() ->
    {setup,
     fun () -> ok = application:start(logi) end,
     fun (_) -> ok = application:stop(logi) end,
     [
      {"The predefined channel",
       fun () ->
               ?assertEqual([logi_channel:default_channel()], logi_channel:which_channels())
       end},
      {"Creates a channel",
       fun () ->
               ?assertEqual(ok,             logi_channel:create(test_channel)),
               ?assertEqual([test_channel], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ?assertEqual(ok,             logi_channel:delete(test_channel)),
               ?assertEqual([],             lists:delete(logi_channel:default_channel(), logi_channel:which_channels()))
       end},
      {"CREATE: If the channel already exists, nothing happens",
       fun () ->
               ok = logi_channel:create(test_channel),
               ?assertEqual([test_channel], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ?assertEqual(ok,             logi_channel:create(test_channel)),
               ?assertEqual([test_channel], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ok = logi_channel:delete(test_channel)
       end},
      {"DELETE: If the channel does exists, it will be silently ignored",
       fun () ->
               ?assertEqual([], lists:delete(logi_channel:default_channel(), logi_channel:which_channels())),
               ?assertEqual(ok, logi_channel:delete(test_channel)),
               ?assertEqual([], lists:delete(logi_channel:default_channel(), logi_channel:which_channels()))
       end},
      {"ERROR(badarg): non atom id",
       fun () ->
               ?assertError(badarg, logi_channel:create("hoge")),
               ?assertError(badarg, logi_channel:delete(<<"fuga">>))
       end},
      {"ERROR(badarg): id conflict",
       fun () ->
               true = register(existing_process_name, spawn(timer, sleep, [infinity])),
               ?assertError(badarg, logi_channel:create(existing_process_name)),

               _ = ets:new(existing_table_name, [named_table]),
               ?assertError(badarg, logi_channel:create(existing_table_name))
       end}
     ]}.
