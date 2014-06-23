%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_backend_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(assertBackend(X), ?assert(logi_backend:is_backend(X))).

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
make_test_() ->
    [
     {"バックエンドオブジェクトが作成できる",
      fun () ->
              Backend = logi_backend:make(backend1, self(), ?MODULE, []),
              ?assertBackend(Backend)
      end},
     {"IDには任意の値を指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(make_ref(), self(), ?MODULE, [])),
              ?assertBackend(logi_backend:make([hoge, "fuga"], self(), ?MODULE, []))
      end},
     {"参照には プロセス名(atom()) or プロセスID(pid()) が指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, [])),
              ?assertBackend(logi_backend:make(id1, name, ?MODULE, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, make_ref(), ?MODULE, [])),
              ?assertError(badarg, logi_backend:make(id1, [hoge, "fuga"], ?MODULE, []))
      end},
     {"モジュールにはモジュール名(atom())が指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, self(), hoge, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, self(), "hoge", []))
      end},
     {"オプションには任意の値が指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, [{key, value}])),
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, non_assoc_list))  % 連想リスト以外もOK
      end}
    ].

get_test_() ->
    [
     {"作成したオブジェクトの各フィールドの値が取得できる",
      fun () ->
              Backend = logi_backend:make(backend1, self(), ?MODULE, []),
              ?assertEqual(backend1, logi_backend:get_id(Backend)),
              ?assertEqual(self(), logi_backend:get_process(Backend)),
              ?assertEqual(?MODULE, logi_backend:get_module(Backend)),
              ?assertEqual([], logi_backend:get_data(Backend))
      end}
    ].
