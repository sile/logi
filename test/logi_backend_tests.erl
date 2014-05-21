%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_backend_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(assertBackend(X), ?assert(logi_backend:is_backend(X))).
-define(INFO, logi_condition:make(info)).

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
make_test_() ->
    [
     {"バックエンドオブジェクトが作成できる",
      fun () ->
              Backend = logi_backend:make(backend1, self(), ?MODULE, ?INFO, []),
              ?assertBackend(Backend)
      end},
     {"IDを省略してバックエンドオブジェクトが作成できる",
      fun () ->
              Backend = logi_backend:make(self(), ?MODULE, ?INFO, []),
              ?assertBackend(Backend),

              %% 省略された場合は、refがidとして使用される
              ?assertEqual(logi_backend:get_id(Backend), logi_backend:get_ref(Backend))
      end},
     {"IDには任意の値を指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(make_ref(), self(), ?MODULE, ?INFO, [])),
              ?assertBackend(logi_backend:make([hoge, "fuga"], self(), ?MODULE, ?INFO, []))
      end},
     {"参照には プロセス名(atom()) or プロセスID(pid()) が指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, ?INFO, [])),
              ?assertBackend(logi_backend:make(id1, name, ?MODULE, ?INFO, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, make_ref(), ?MODULE, ?INFO, [])),
              ?assertError(badarg, logi_backend:make(id1, [hoge, "fuga"], ?MODULE, ?INFO, []))
      end},
     {"モジュールにはモジュール名(atom())が指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, self(), hoge, ?INFO, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, self(), "hoge", ?INFO, []))
      end},
     {"条件指定には logi:condition() 型の値が定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, ?INFO, [])),
              
              %% NG
              ?assertError(badarg, logi_backend:make(id1, self(), ?MODULE, hoge, []))
      end},
     {"オプションには任意の値が指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, ?INFO, [{key, value}])),
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, ?INFO, non_assoc_list))  % 連想リスト以外もOK
      end}
    ].

get_test_() ->
    [
     {"作成したオブジェクトの各フィールドの値が取得できる",
      fun () ->
              Backend = logi_backend:make(backend1, self(), ?MODULE, ?INFO, []),
              ?assertEqual(backend1, logi_backend:get_id(Backend)),
              ?assertEqual(self(), logi_backend:get_ref(Backend)),
              ?assertEqual(?MODULE, logi_backend:get_module(Backend)),
              ?assertEqual(?INFO, logi_backend:get_condition(Backend)),
              ?assertEqual([], logi_backend:get_data(Backend))
      end}
    ].
