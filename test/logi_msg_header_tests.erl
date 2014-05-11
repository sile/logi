%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc `log_msg_header'のテストモジュール
-module(logi_msg_header_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
empty_test_() ->
    [
     {"空のヘッダ情報が生成できる",
      fun () ->
              Header = logi_msg_header:empty(),
              ?assert(logi_msg_header:is_instance(Header)),
              ?assertEqual([], logi_msg_header:get_entries([], Header))
      end}
    ].

from_list_test_() ->
    [
     {"連想リストからヘッダ情報が生成できる",
      fun () ->
              List = [
                      {process,    [{a, 10}, {b, 20}]},
                      {{key, val}, [{c, 30}]}
                     ],
              Header = logi_msg_header:from_list(List),
              ?assert(logi_msg_header:is_instance(Header)),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_header:get_entries([], Header)),
              ?assertEqual([{a, 10}, {b, 20}, {c, 30}], logi_msg_header:get_entries([{key, val}], Header))
      end},
     {"空リストからヘッダ情報が生成できる",
      fun () ->
              ?assertEqual(logi_msg_header:empty(), logi_msg_header:from_list([])),

              %% 値が空の場合も同様
              ?assertEqual(logi_msg_header:empty(), logi_msg_header:from_list([{process, []}]))
      end},
     {"不正な引数を渡した場合はエラーが送出される",
      fun () ->
              %% 非リスト
              ?assertError(function_clause, logi_msg_header:from_list(atom)),

              %% 非連想リスト
              ?assertError(function_clause, logi_msg_header:from_list([atom])),

              %% キーがscope()型ではない
              ?assertError({assertion_failed, _}, logi_msg_header:from_list([{10, []}])),

              %% 値が[logi:header_entry()] 型ではない
              ?assertError({assertion_failed, _}, logi_msg_header:from_list([{process, 10}])),
              ?assertError({assertion_failed, _}, logi_msg_header:from_list([{process, [10]}])),
              ?assertError({assertion_failed, _}, logi_msg_header:from_list([{process, [{1, 2, 3}]}]))
      end}
    ].

set_and_get_entries_test_() ->
    [
     {"ヘッダ情報の設定/取得が行える",
      fun () ->
              Header0 = logi_msg_header:empty(),
              ?assertEqual([], logi_msg_header:get_entries([], Header0)),

              Header1 = logi_msg_header:set_entries(process, [{a, 10}], Header0),
              ?assertEqual([{a, 10}], logi_msg_header:get_entries([], Header1))
      end},
     {"スコープつきでヘッダ情報の設定/取得が行える",
      fun () ->
              Header0 = logi_msg_header:from_list([{process, [{a, 10}]}]),

              Scope1 = {scope, 1},
              Header1 = logi_msg_header:set_entries(Scope1, [{b, 20}], Header0),
              ?assertEqual([{a, 10}], logi_msg_header:get_entries([], Header1)),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_header:get_entries([Scope1], Header1)),

              Scope2 = {scope, 2},
              Header2 = logi_msg_header:set_entries(Scope2, [{c, 30}], Header1),
              ?assertEqual([{a, 10}], logi_msg_header:get_entries([], Header2)),
              ?assertEqual([{a, 10}, {c, 30}], logi_msg_header:get_entries([Scope2], Header2)),

              ?assertEqual([{a, 10}, {b, 20}, {c, 30}], logi_msg_header:get_entries([Scope1, Scope2], Header2))
      end},
     {"すでに存在するキーが指定された場合は、値が上書きされる",
      fun () ->
              Header0 = logi_msg_header:from_list([{process, [{a, 10}]}]),
              ?assertEqual([{a, 10}], logi_msg_header:get_entries([], Header0)),

              Header1 = logi_msg_header:set_entries(process, [{a, 20}], Header0),
              ?assertEqual([{a, 20}], logi_msg_header:get_entries([], Header1))
      end},
     {"スコープが異なる場合は、キーが同じでも値は上書きされない",
      fun () ->
              Header0 = logi_msg_header:from_list([{{scope, 1}, [{a, 10}]},
                                                   {{scope, 2}, [{a, 20}]}]),
              ?assertEqual([{a, 10}], logi_msg_header:get_entries([{scope, 1}], Header0)),
              ?assertEqual([{a, 20}], logi_msg_header:get_entries([{scope, 2}], Header0)),

              Header1 = logi_msg_header:set_entries({scope, 1}, [{a, 30}], Header0),
              ?assertEqual([{a, 30}], logi_msg_header:get_entries([{scope, 1}], Header1)), % こっちだけ更新される
              ?assertEqual([{a, 20}], logi_msg_header:get_entries([{scope, 2}], Header1))
      end}
    ].

unset_entries_test_() ->
    [
     {"ヘッダ情報の削除(部分削除)が行える",
      fun () ->
              Header0 = logi_msg_header:from_list([{process, [{a, 10}, {b, 20}]}]),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_header:get_entries([], Header0)),

              Header1 = logi_msg_header:unset_entries(process, [a], Header0),
              ?assertEqual([{b, 20}], logi_msg_header:get_entries([], Header1))
      end},
     {"ヘッダ情報の削除(部分削除)はスコープ単位で行える",
      fun () ->
              Header0 = logi_msg_header:from_list([{{scope, 1}, [{a, 10}]},
                                                   {{scope, 2}, [{a, 20}]}]),
              ?assertEqual([{a, 10}], logi_msg_header:get_entries([{scope, 1}], Header0)),
              ?assertEqual([{a, 20}], logi_msg_header:get_entries([{scope, 2}], Header0)),
              
              Header1 = logi_msg_header:unset_entries({scope, 1}, [a], Header0),
              ?assertEqual([],        logi_msg_header:get_entries([{scope, 1}], Header1)), % こっちだけ削除される
              ?assertEqual([{a, 20}], logi_msg_header:get_entries([{scope, 2}], Header1))
      end},
     {"存在しないスコープを指定してもエラーにはならない",
      fun () ->
              Header0 = logi_msg_header:from_list([{process, [{a, 10}, {b, 20}]}]),
              Header1 = logi_msg_header:unset_entries({scope, 1}, [a], Header0),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_header:get_entries([], Header1))
      end},
     {"存在しないキーを指定してもエラーにはならない",
      fun () ->
              Header0 = logi_msg_header:from_list([{process, [{a, 10}, {b, 20}]}]),
              Header1 = logi_msg_header:unset_entries(process, [c], Header0),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_header:get_entries([], Header1))
      end}
    ].

erase_entries_test_() ->
    [
     {"ヘッダ情報の削除(スコープ単位での全削除)が行える",
      fun () ->
              Header0 = logi_msg_header:from_list([{{scope, 1}, [{a, 10}, {b, 10}]},
                                                   {{scope, 2}, [{a, 20}, {b, 20}]}]),
              ?assertEqual([{a, 10}, {b, 10}], logi_msg_header:get_entries([{scope, 1}], Header0)),
              ?assertEqual([{a, 20}, {b, 20}], logi_msg_header:get_entries([{scope, 2}], Header0)),
              
              Header1 = logi_msg_header:erase_entries({scope, 1}, Header0),
              ?assertEqual([],                 logi_msg_header:get_entries([{scope, 1}], Header1)), % こっちだけ削除される
              ?assertEqual([{a, 20}, {b, 20}], logi_msg_header:get_entries([{scope, 2}], Header1))
      end},
     {"存在しないスコープを指定してもエラーにはならない",
      fun () ->
              Header0 = logi_msg_header:from_list([{process, [{a, 10}, {b, 20}]}]),
              Header1 = logi_msg_header:erase_entries({scope, 1}, Header0),
              ?assertEqual([{a, 10}, {b, 20}], logi_msg_header:get_entries([], Header1))
      end}
    ].
