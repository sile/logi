%% coding: latin-1
%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(CONTEXT, 'TEST_CONTEXT_ID').

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
headers_test_() ->
    [
     {"ヘッダ群が設定できる",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_headers(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(Context1))
      end},
     {"ヘッダ群が設定できる(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_headers(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(?CONTEXT))
      end},
     {"ヘッダ群の更新(上書き)ができる",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_headers(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(Context1)),

              Context2 = logi:set_headers(Context1, [{b, b}, {c, c}]),
              ?assertEqual([{a, 10}, {b, b}, {c, c}], logi:get_headers(Context2))
      end},
     {"ヘッダ群の更新(上書き)ができる(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_headers(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(?CONTEXT)),

              ?CONTEXT = logi:set_headers(?CONTEXT, [{b, b}, {c, c}]),
              ?assertEqual([{a, 10}, {b, b}, {c, c}], logi:get_headers(?CONTEXT))
      end},
     {"ヘッダの削除が行える",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_headers(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(Context1)),

              Context2 = logi:delete_headers(Context1, [b]),
              ?assertEqual([{a, 10}], logi:get_headers(Context2))
      end},
     {"ヘッダの削除が行える(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_headers(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(?CONTEXT)),

              ?CONTEXT = logi:delete_headers(?CONTEXT, [b]),
              ?assertEqual([{a, 10}], logi:get_headers(?CONTEXT))
      end},
     {"ヘッダのクリアが行える",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_headers(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(Context1)),

              Context2 = logi:clear_headers(Context1),
              ?assertEqual([], logi:get_headers(Context2))
      end},
     {"ヘッダのクリアが行える(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_headers(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_headers(?CONTEXT)),

              ?CONTEXT = logi:clear_headers(?CONTEXT),
              ?assertEqual([], logi:get_headers(?CONTEXT))
      end}
    ].

metadata_test_() ->
    [
     {"メタデータ群が設定できる",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_metadata(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(Context1))
      end},
     {"メタデータ群が設定できる(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_metadata(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(?CONTEXT))
      end},
     {"メタデータ群の更新(上書き)ができる",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_metadata(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(Context1)),

              Context2 = logi:set_metadata(Context1, [{b, b}, {c, c}]),
              ?assertEqual([{a, 10}, {b, b}, {c, c}], logi:get_metadata(Context2))
      end},
     {"メタデータ群の更新(上書き)ができる(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_metadata(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(?CONTEXT)),

              ?CONTEXT = logi:set_metadata(?CONTEXT, [{b, b}, {c, c}]),
              ?assertEqual([{a, 10}, {b, b}, {c, c}], logi:get_metadata(?CONTEXT))
      end},
     {"メタデータの削除が行える",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_metadata(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(Context1)),

              Context2 = logi:delete_metadata(Context1, [b]),
              ?assertEqual([{a, 10}], logi:get_metadata(Context2))
      end},
     {"メタデータの削除が行える(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_metadata(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(?CONTEXT)),

              ?CONTEXT = logi:delete_metadata(?CONTEXT, [b]),
              ?assertEqual([{a, 10}], logi:get_metadata(?CONTEXT))
      end},
     {"メタデータのクリアが行える",
      fun () ->
              Context0 = logi:make_context(),
              Context1 = logi:set_metadata(Context0, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(Context1)),

              Context2 = logi:clear_metadata(Context1),
              ?assertEqual([], logi:get_metadata(Context2))
      end},
     {"メタデータのクリアが行える(プロセス辞書版)",
      fun () ->
              _ = erase(),
              ?CONTEXT = logi:set_metadata(?CONTEXT, [{a, 10}, {b, 20}]),
              ?assertEqual([{a, 10}, {b, 20}], logi:get_metadata(?CONTEXT)),

              ?CONTEXT = logi:clear_metadata(?CONTEXT),
              ?assertEqual([], logi:get_metadata(?CONTEXT))
      end}
    ].
