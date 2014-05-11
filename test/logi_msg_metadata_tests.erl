%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc `log_msg_metadata'のテストモジュール
-module(logi_msg_metadata_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
empty_test_() ->
    [
     {"空のメタデータ情報が生成できる",
      fun () ->
              MetaData = logi_msg_metadata:empty(),
              ?assert(logi_msg_metadata:is_instance(MetaData)),
              ?assertEqual([], logi_msg_metadata:get_entries(MetaData))
      end}
    ].

from_list_test_() ->
    [
     {"連想リストからメタデータ情報が生成できる",
      fun () ->
              List = [{a, 10}, {b, 20}],
              MetaData = logi_msg_metadata:from_list(List),
              ?assert(logi_msg_metadata:is_instance(MetaData)),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_metadata:get_entries(MetaData))
      end},
     {"空リストからメタデータ情報が生成できる",
      fun () ->
              ?assertEqual(logi_msg_metadata:empty(), logi_msg_metadata:from_list([]))
      end},
     {"不正な引数を渡した場合はエラーが送出される",
      fun () ->
              %% 非リスト
              ?assertError(function_clause, logi_msg_metadata:from_list(atom)),

              %% 非連想リスト
              ?assertError({assertion_failed, _}, logi_msg_metadata:from_list([atom]))
      end}
    ].

set_and_get_entries_test_() ->
    [
     {"メタデータ情報の設定/取得が行える",
      fun () ->
              Metadata0 = logi_msg_metadata:empty(),
              ?assertEqual([], logi_msg_metadata:get_entries( Metadata0)),

              Metadata1 = logi_msg_metadata:set_entries([{a, 10}], Metadata0),
              ?assertEqual([{a, 10}], logi_msg_metadata:get_entries(Metadata1))
      end},
     {"すでに存在するキーが指定された場合は、値が上書きされる",
      fun () ->
              Metadata0 = logi_msg_metadata:from_list([{a, 10}]),
              ?assertEqual([{a, 10}], logi_msg_metadata:get_entries(Metadata0)),

              Metadata1 = logi_msg_metadata:set_entries([{a, 20}], Metadata0),
              ?assertEqual([{a, 20}], logi_msg_metadata:get_entries(Metadata1))
      end}
    ].

unset_entries_test_() ->
    [
     {"メタデータ情報の削除が行える",
      fun () ->
              MetaData0 = logi_msg_metadata:from_list([{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_metadata:get_entries(MetaData0)),

              MetaData1 = logi_msg_metadata:unset_entries([a], MetaData0),
              ?assertEqual([{b, 20}], logi_msg_metadata:get_entries(MetaData1))
      end},
     {"存在しないキーを指定してもエラーにはならない",
      fun () ->
              MetaData0 = logi_msg_metadata:from_list([{a, 10}, {b, 20}]),
              MetaData1 = logi_msg_metadata:unset_entries([c], MetaData0),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_metadata:get_entries(MetaData1))
      end}
    ].
