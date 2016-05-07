%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_name_server_tests).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
register_and_unregister_test_() ->
    {foreach,
     fun ()  ->
             {ok, _} = logi_name_server:start_link()
     end,
     fun (_) ->
             ok = gen_server:stop(logi_name_server)
     end,
     [
      {"register process name",
       fun () ->
               ?assertEqual(yes, logi_name_server:register_name(foo, self())),
               ?assertEqual(self(), logi_name_server:whereis_name(foo))
       end},
      {"duplicated registration",
       fun () ->
               yes = logi_name_server:register_name(foo, self()),
               ?assertEqual(no, logi_name_server:register_name(foo, self())),
               ?assertEqual(no, logi_name_server:register_name(bar, self()))
       end},
      {"unregister process name",
       fun () ->
               yes = logi_name_server:register_name(foo, self()),

               ?assertEqual(ok, logi_name_server:unregister_name(foo)),
               ?assertEqual(yes, logi_name_server:register_name(foo, self())),

               ?assertEqual(ok, logi_name_server:unregister_name(foo)),
               ?assertEqual(yes, logi_name_server:register_name(bar, self()))
       end},
      {"unregister not-registered name",
       fun () ->
               ?assertEqual(ok, logi_name_server:unregister_name(foo))
       end}
     ]}.

whereis_test_() ->
    {foreach,
     fun ()  ->
             {ok, _} = logi_name_server:start_link()
     end,
     fun (_) ->
             ok = gen_server:stop(logi_name_server)
     end,
     [
      {"resolve name",
       fun () ->
               yes = logi_name_server:register_name(foo, self()),
               ?assertEqual(self(), logi_name_server:whereis_name(foo))
       end},
      {"resolve not-registered name",
       fun () ->
               ?assertEqual(undefined, logi_name_server:whereis_name(foo))
       end},
      {"resolve unregistered name",
       fun () ->
               yes = logi_name_server:register_name(foo, self()),
               ok = logi_name_server:unregister_name(foo),
               ?assertEqual(undefined, logi_name_server:whereis_name(foo))
       end}
     ]}.

process_down_test_() ->
    {foreach,
     fun ()  ->
             {ok, _} = logi_name_server:start_link()
     end,
     fun (_) ->
             ok = gen_server:stop(logi_name_server)
     end,
     [
      {"register downed process",
       fun () ->
               process_flag(trap_exit, true),

               Pid = spawn_link(fun () -> ok end),
               receive {'EXIT', Pid, _} -> ok end,

               yes = logi_name_server:register_name(foo, Pid),
               ?assertEqual(undefined, logi_name_server:whereis_name(foo)),

               yes = logi_name_server:register_name(foo, self()),
               ?assertEqual(self(), logi_name_server:whereis_name(foo))
       end},
      {"register then process down",
       fun () ->
               process_flag(trap_exit, true),

               Pid = spawn_link(timer, sleep, [infinity]),

               yes = logi_name_server:register_name(foo, Pid),
               ?assertEqual(Pid, logi_name_server:whereis_name(foo)),

               exit(Pid, shutdown),
               receive {'EXIT', Pid, _} -> ok end,

               ?assertEqual(undefined, logi_name_server:whereis_name(foo))
       end}
     ]}.
