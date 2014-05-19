%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_condition_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(assertCond(X), ?assert(logi_condition:is_condition(X))).

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
make_test_() ->
    [
     {"ログレベルを指定して、条件オブジェクトが作成できる",
      fun () ->
              ?assertCond(logi_condition:make(info))
      end},
     {"制約付きのログレベルを指定して、条件オブジェクトが作成できる",
      fun () ->
              %% hoge:fuga(pattern, [logi:metadata_entry()]) が true を返した場合にのみログが出力される
              ?assertCond(logi_condition:make({info, {match, {hoge, fuga, pattern}}})),

              %% 制約無し
              ?assertCond(logi_condition:make({info, none}))
      end},
     {"複数条件を組み合わせて指定可能",
      fun () ->
              Spec = [
                      info,                                    % info以上は常に出力
                      {debug, {match, {hoge, fuga, pattern}}}  % 制約を満たしている場合にのみdebug以上も出力
                     ],
              ?assertCond(logi_condition:make(Spec))
      end},
     {"ログレベルを複数指定してもエラーにはならない",
      fun () ->
              Spec = [info, debug, verbose, warning, alert],
              ?assertCond(logi_condition:make(Spec))
      end},
     {"条件指定には logi:condition_spec() 型の値が定可能",
      fun () ->
              %% OK
              Spec1 = info, % ログレベルのみ指定
              ?assertCond(logi_condition:make(Spec1)),
              
              Spec2 = {debug, {match, {module_name, function_name, ["hoge"]}}}, % マッチ条件を指定
              ?assertCond(logi_condition:make(Spec2)),

              Spec3 = [Spec1, Spec2],  % 複数ログレベルを条件によって出し分け
              ?assertCond(logi_condition:make(Spec3)),

              ?assertCond(logi_condition:make([])), % 空リストもok
              
              %% NG
              ?assertError(badarg, logi_condition:make(erlang)),            % 存在しないログレベル
              ?assertError(badarg, logi_condition:make({info, always, 3})), % 不正なサイズのタプル
              ?assertError(badarg, logi_condition:make({info, hoge})),      % 存在しない条件
              ?assertError(badarg, logi_condition:make(make_ref())),        % 不正な値
              ?assertError(badarg, logi_condition:make([[info]]))           % ネストしたリスト
      end}
    ].

get_test_() ->
    [
     {"条件指定が取得できる",
      fun () ->
              ?assertEqual(info, logi_condition:get_spec(logi_condition:make(info)))
      end},
     {"正規化された条件指定が取得できる",
      fun () ->
              ?assertEqual([{info,none}], logi_condition:get_normalized_spec(logi_condition:make(info))),  % リスト形式に統一される
              ?assertEqual([{info,none}], logi_condition:get_normalized_spec(logi_condition:make([info]))),
              ?assertEqual([{info,none}], logi_condition:get_normalized_spec(logi_condition:make([{info, none}])))
      end}
    ].
