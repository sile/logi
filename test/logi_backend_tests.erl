%% coding: latin-1
%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_backend_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(assertBackend(X), ?assert(logi_backend:is_backend(X))).

-define(BACKEND_PROCESS_NAME, hoge). % dummy name

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
make_test_() ->
    [
     {"バックエンドオブジェクトが作成できる",
      fun () ->
              Backend = logi_backend:make(backend1, ?BACKEND_PROCESS_NAME, ?MODULE, []),
              ?assertBackend(Backend)
      end},
     {"IDには任意の値を指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(make_ref(), ?BACKEND_PROCESS_NAME, ?MODULE, [])),
              ?assertBackend(logi_backend:make([hoge, "fuga"], ?BACKEND_PROCESS_NAME, ?MODULE, []))
      end},
     {"バックエンドプロセスには名前付きプロセス(atom())のみが指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, ?BACKEND_PROCESS_NAME, ?MODULE, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, self(), ?MODULE, [])),
              ?assertError(badarg, logi_backend:make(id1, make_ref(), ?MODULE, []))
      end},
     {"モジュールにはモジュール名(atom())が指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, ?BACKEND_PROCESS_NAME, hoge, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, ?BACKEND_PROCESS_NAME, "hoge", []))
      end},
     {"オプションには任意の値が指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(id1, ?BACKEND_PROCESS_NAME, ?MODULE, [{key, value}])),
              ?assertBackend(logi_backend:make(id1, ?BACKEND_PROCESS_NAME, ?MODULE, non_assoc_list))  % 連想リスト以外もOK
      end}
    ].

get_test_() ->
    [
     {"作成したオブジェクトの各フィールドの値が取得できる",
      fun () ->
              Backend = logi_backend:make(backend1, ?BACKEND_PROCESS_NAME, ?MODULE, []),
              ?assertEqual(backend1, logi_backend:get_id(Backend)),
              ?assertEqual(?BACKEND_PROCESS_NAME, logi_backend:get_process(Backend)),
              ?assertEqual(?MODULE, logi_backend:get_module(Backend)),
              ?assertEqual([], logi_backend:get_data(Backend))
      end}
    ].
