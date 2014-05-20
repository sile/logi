%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_frequency_control_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
is_logging_turn_test_() ->
    {foreach, spawn,    %% 毎回異なるプロセスで実行したいのでforeachを使う
     fun ()  -> ok end,
     fun (_) -> ok end,
     [
      %% policy: always
      {"policy=always の場合は常に出力順がまわってくる",
       fun () ->
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(always, id)),
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(always, id)),
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(always, id))
       end},

      %% policy: once
      {"policy=once の場合は最初の一回だけが許可される",
       fun () ->
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(once, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn(once, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn(once, id))
       end},

      %% policy: {interval_count, N}
      {"policy={interval_count,N} の場合は、N+1回に一回だけ許可される",
       fun () ->
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn({interval_count, 2}, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn({interval_count, 2}, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn({interval_count, 2}, id)),
               ?assertEqual({true, 2}, logi_frequency_control:is_logging_turn({interval_count, 2}, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn({interval_count, 2}, id))
       end},

      %% policy: {interval_time, MilliSec}
      {"policy={interval_count, MilliSec} の場合は、MilliSec間に一回だけ許可される",
       fun () ->
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn({interval_time, 10}, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn({interval_time, 10}, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn({interval_time, 10}, id)),
               timer:sleep(10),
               ?assertEqual({true, 2}, logi_frequency_control:is_logging_turn({interval_time, 10}, id))
       end},

      %% 影響範囲について
      {"頻度調整の影響範囲は同じプロセス内のみ",
       fun () ->
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(once, id)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn(once, id)),
               Pid = spawn(fun () ->
                                   ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(once, id)),
                                   ?assertEqual(false,     logi_frequency_control:is_logging_turn(once, id))
                           end),
               monitor(process, Pid),
               receive
                   {'DOWN', _, _, Pid, _} -> ok
               end
       end},
      {"頻度調整の影響範囲は同じIDのもののみ",
       fun () ->
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(once, hoge)),
               ?assertEqual({true, 0}, logi_frequency_control:is_logging_turn(once, fuga)),
               ?assertEqual(false,     logi_frequency_control:is_logging_turn(once, hoge))
       end}
     ]}.
