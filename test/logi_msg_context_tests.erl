%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc `log_msg_context'のテストモジュール
-module(logi_msg_context_tests).

-include_lib("eunit/include/eunit.hrl").
-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
save_and_load_context_test_() ->
    {foreach,
     fun () -> ok end,
     fun (_) -> ok = logi_msg_context:erase_context(logi_msg_header) end,
     [
      {"空のヘッダコンテキストの保存および読込が行える",
       fun () ->
               Context = logi_msg_context:load_context(logi_msg_header),
               ?assertEqual([], Context),
               ?assertEqual(ok, logi_msg_context:save_context(logi_msg_header, Context))
       end},
      {"内容が更新されたヘッダコンテキストの保存および読込が行える",
       fun () ->
               ?assertEqual([], logi_msg_context:load_context(logi_msg_header)),

               Context0 = [{?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header:empty()}],
               ?assertEqual(ok, logi_msg_context:save_context(logi_msg_header, Context0)),
               ?assertEqual(Context0, logi_msg_context:load_context(logi_msg_header)),

               Context1 = [{self(), logi_msg_header:from_list([{process, [{a, 10}]}])} |
                           logi_msg_context:load_context(logi_msg_header)],
               ?assertEqual(ok, logi_msg_context:save_context(logi_msg_header, Context1)),
               ?assertEqual(Context1, logi_msg_context:load_context(logi_msg_header))
       end},
      {"コンテキストの保存/読込はプロセス単位で独立して行われる",
       fun () ->
               ?assertEqual([], logi_msg_context:load_context(logi_msg_header)),

               Context0 = [{?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header:empty()}],
               ?assertEqual(ok, logi_msg_context:save_context(logi_msg_header, Context0)),
               ?assertEqual(Context0, logi_msg_context:load_context(logi_msg_header)),
               monitor(process,
                       spawn(fun () ->
                                     %% 親プロセスのコンテキストの内容には影響を受けない
                                     ?assertEqual([], logi_msg_context:load_context(logi_msg_header))
                             end)),
               receive
                   {'DOWN', _, _, _, _} -> ok
               end
       end},
      {"保存しようとされているコンテキストの形式が不正な場合は、エラーが送出される",
       fun () ->
               Context0 = wrong_context,
               ?assertError({wrong_context, _}, logi_msg_context:save_context(logi_msg_header, Context0)),
               ?assertEqual([], logi_msg_context:load_context(logi_msg_header)),

               Context1 = [{wrong_context_key, wrong_context_value}],
               ?assertError({assertion_failed, _}, logi_msg_context:save_context(logi_msg_header, Context1)),
               ?assertEqual([], logi_msg_context:load_context(logi_msg_header)),

               Context2 = [wrong_context_entry],
               ?assertError({wrong_context_entry, _}, logi_msg_context:save_context(logi_msg_header, Context2)),
               ?assertEqual([], logi_msg_context:load_context(logi_msg_header))
       end}
     ]}.

erase_context_test_() ->
    [
     {"空のヘッダコンテキストの削除が行える",
      fun () ->
              ?assertEqual([], logi_msg_context:load_context(logi_msg_header)),
              ?assertEqual(ok, logi_msg_context:erase_context(logi_msg_header)),
              ?assertEqual([], logi_msg_context:load_context(logi_msg_header))
      end},
     {"保存されたヘッダコンテキストの削除が行える",
      fun () ->
              Context = [{?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header:empty()}],
              ?assertEqual(ok, logi_msg_context:save_context(logi_msg_header, Context)),
              ?assertEqual(Context, logi_msg_context:load_context(logi_msg_header)),

              ?assertEqual(ok, logi_msg_context:erase_context(logi_msg_header)),
              ?assertEqual([], logi_msg_context:load_context(logi_msg_header))
      end}
    ].

get_header_test_() ->
    [
     {"コンテキストからヘッダ情報が取り出せる",
      fun () ->
              Context = dummy_context(),
              ?assertEqual(dummy_header_for_default_event_manager(),
                           logi_msg_context:get_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context))
      end},
     {"対応するログ情報が存在しないイベントマネージャが指定された場合は空のヘッダ情報が返される",
      fun () ->
              Context = [],
              ?assertEqual(logi_msg_header:empty(),
                           logi_msg_context:get_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context))
      end}
    ].

delete_header_test_() ->
    [
     {"コンテキストからヘッダ情報が削除できる",
      fun () ->
              Context0 = dummy_context(),
              ?assertEqual(dummy_header_for_default_event_manager(),
                           logi_msg_context:get_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context0)),

              Context1 = logi_msg_context:delete_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context0),

              %% 削除されたのでデフォルトのヘッダ情報(空ヘッダ)が返るようになる
              ?assertEqual(logi_msg_header:empty(),
                           logi_msg_context:get_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context1))
      end},
     {"存在しないイベントマネージャに対応するヘッダ情報を削除しようとしてもエラーにはならない",
      fun () ->
              Context = [],
              ?assertEqual(Context, logi_msg_context:delete_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context))
      end}
    ].

update_header_test_() ->
    [
     {"コンテキスト内のヘッダ情報が更新できる",
      fun () ->
              Context0 = dummy_context(),
              ?assertEqual(dummy_header_for_default_event_manager(),
                           logi_msg_context:get_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context0)),

              NewHeader = logi_msg_header:from_list([{process, [{b, 20}]}]),
              Context1 = logi_msg_context:update_info(
                           ?LOGI_DEFAULT_EVENT_MANAGER,
                           fun (OldHeader) ->
                                   ?assertEqual(dummy_header_for_default_event_manager(), OldHeader),
                                   NewHeader
                           end,
                           logi_msg_header,
                           Context0),
              
              ?assertEqual(NewHeader,
                           logi_msg_context:get_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context1))
      end},
     {"存在しないイベントマネージャに対応するヘッダ情報を更新しようとした場合は、古いヘッダ情報として空ヘッダが渡される",
      fun () ->
              Context0 = [],
              NewHeader = logi_msg_header:from_list([{process, [{b, 20}]}]),
              Context1 = logi_msg_context:update_info(
                           ?LOGI_DEFAULT_EVENT_MANAGER,
                           fun (OldHeader) ->
                                   ?assertEqual(logi_msg_header:empty(), OldHeader),
                                   NewHeader
                           end,
                           logi_msg_header,
                           Context0),
              
              ?assertEqual(NewHeader,
                           logi_msg_context:get_info(?LOGI_DEFAULT_EVENT_MANAGER, logi_msg_header, Context1))
      end}
    ].

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
dummy_header_for_default_event_manager() ->
    logi_msg_header:from_list([{process, [{a, 10}]}]).

dummy_context() ->
    [
     {?LOGI_DEFAULT_EVENT_MANAGER, dummy_header_for_default_event_manager()},
     {empty, logi_msg_header:empty()}
    ].
