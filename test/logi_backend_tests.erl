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
              Backend = logi_backend:make(backend1, self(), ?MODULE, info, []),
              ?assertBackend(Backend)
      end},
     {"IDを省略してバックエンドオブジェクトが作成できる",
      fun () ->
              Backend = logi_backend:make(self(), ?MODULE, info, []),
              ?assertBackend(Backend),

              %% 省略された場合は、refがidとして使用される
              ?assertEqual(logi_backend:get_id(Backend), logi_backend:get_ref(Backend))
      end},
     {"作成したオブジェクトの各フィールドの値が取得できる",
      fun () ->
              Backend = logi_backend:make(backend1, self(), ?MODULE, info, []),
              ?assertEqual(backend1, logi_backend:get_id(Backend)),
              ?assertEqual(self(), logi_backend:get_ref(Backend)),
              ?assertEqual(?MODULE, logi_backend:get_module(Backend)),
              ?assertEqual(info, logi_backend:get_condition_spec(Backend)),
              ?assertEqual([], logi_backend:get_options(Backend))
      end},
     {"IDには任意の値を指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(make_ref(), self(), ?MODULE, info, [])),
              ?assertBackend(logi_backend:make([hoge, "fuga"], self(), ?MODULE, info, []))
      end},
     {"参照には プロセス名(atom()) or プロセスID(pid()) が指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, info, [])),
              ?assertBackend(logi_backend:make(id1, name, ?MODULE, info, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, make_ref(), ?MODULE, info, [])),
              ?assertError(badarg, logi_backend:make(id1, [hoge, "fuga"], ?MODULE, info, []))
      end},
     {"モジュールにはモジュール名(atom())が指定可能",
      fun () ->
              %% OK
              ?assertBackend(logi_backend:make(id1, self(), hoge, info, [])),

              %% NG
              ?assertError(badarg, logi_backend:make(id1, self(), "hoge", info, []))
      end},
     {"条件指定には logi:condition_spec() 型の値が定可能",
      fun () ->
              %% OK
              Spec1 = info, % ログレベルのみ指定
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, Spec1, [])),
              
              Spec2 = {debug, always}, % ログレベルのみ指定と同じ効果
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, Spec2, [])),

              Spec3 = {debug, {match, {module_name, function_name, ["hoge"]}}}, % マッチ条件を指定
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, Spec3, [])),

              Spec4 = [Spec1, Spec3],  % 複数ログレベルを条件によって出し分け
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, Spec4, [])),

              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, [], [])), % 空リストもok
              
              %% NG
              ?assertError(badarg, logi_backend:make(id1, self(), ?MODULE, erlang, [])),            % 存在しないログレベル
              ?assertError(badarg, logi_backend:make(id1, self(), ?MODULE, {info, always, 3}, [])), % 不正なサイズのタプル
              ?assertError(badarg, logi_backend:make(id1, self(), ?MODULE, {info, none}, [])),      % 存在しない条件
              ?assertError(badarg, logi_backend:make(id1, self(), ?MODULE, make_ref(), [])),        % 不正な値
              ?assertError(badarg, logi_backend:make(id1, self(), ?MODULE, [[info]], []))           % ネストしたリスト
      end},
     {"オプションには任意の値が指定可能",
      fun () ->
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, info, [{key, value}])),
              ?assertBackend(logi_backend:make(id1, self(), ?MODULE, info, non_assoc_list))  % 連想リスト以外もOK
      end}
    ].
