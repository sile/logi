%% coding: latin-1
%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_backend_table_tests).

-include_lib("eunit/include/eunit.hrl").

-export([metadata_member/4]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(INFO, logi_condition:make(info)).

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
new_and_delete_test_() ->
    [
     {"名前付きバックエンドテーブルの作成および削除が行える",
      fun () ->
              _ = logi_backend_table:new(test_table),
              ?assert(lists:member(test_table, ets:all())),

              ok = logi_backend_table:delete(test_table),
              ?assert(not lists:member(test_table, ets:all()))
      end}
    ].

register_backend_test_() ->
    {foreach, local,
     fun () -> test = logi_backend_table:new(test) end,
     fun (_) -> ok = logi_backend_table:delete(test) end,
     [
      {"新規バックエンドを登録できる",
       fun () ->
               Backend = logi_backend:make(backend1, self(), ?MODULE, []),
               ?assertEqual(ok, logi_backend_table:register_backend(test, ?INFO, Backend))
       end},
      {"IDが重複したバックエンドを登録した場合は、後のものの内容が優先される",
       fun () ->
               Backend1 = logi_backend:make(backend1, self(), ?MODULE, []),
               ?assertEqual(ok, logi_backend_table:register_backend(test, ?INFO, Backend1)),

               Backend2 = logi_backend:make(backend1, name, moulde, [hoge,fuga]),
               ?assertEqual(ok, logi_backend_table:register_backend(test, ?INFO, Backend2))
       end}
     ]}.

find_backend_test_() ->
    {foreach, local,
     fun () -> test = logi_backend_table:new(test) end,
     fun (_) -> ok = logi_backend_table:delete(test) end,
     [
      {"登録したバックエンドを検索できる",
       fun () ->
               Backend1 = logi_backend:make(backend1, self(), ?MODULE, []),
               Backend2 = logi_backend:make(backend2, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, ?INFO, Backend1),
               ok = logi_backend_table:register_backend(test, ?INFO, Backend2),

               ?assertEqual({ok, Backend1}, logi_backend_table:find_backend(test, logi_backend:get_id(Backend1))),
               ?assertEqual({ok, Backend2}, logi_backend_table:find_backend(test, logi_backend:get_id(Backend2)))
       end},
      {"存在しないIDが指定された場合は`error'が返る",
       fun () ->
               Backend = logi_backend:make(backend1, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, ?INFO, Backend),

               ?assertEqual(error, logi_backend_table:find_backend(test, backend2))
       end}
     ]}.

deregister_backend_test_() ->
    {foreach, local,
     fun () -> test = logi_backend_table:new(test) end,
     fun (_) -> ok = logi_backend_table:delete(test) end,
     [
      {"バックエンドの登録を解除できる",
       fun () ->
               Backend1 = logi_backend:make(backend1, self(), ?MODULE, []),
               Backend2 = logi_backend:make(backend2, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, ?INFO, Backend1),
               ok = logi_backend_table:register_backend(test, ?INFO, Backend2),

               ?assertEqual({ok, Backend1}, logi_backend_table:find_backend(test, logi_backend:get_id(Backend1))),
               ?assertEqual(ok, logi_backend_table:deregister_backend(test, logi_backend:get_id(Backend1))),
               ?assertEqual(error, logi_backend_table:find_backend(test, logi_backend:get_id(Backend1))),

               ?assertEqual({ok, Backend2}, logi_backend_table:find_backend(test, logi_backend:get_id(Backend2))),
               ?assertEqual(ok, logi_backend_table:deregister_backend(test, logi_backend:get_id(Backend2))),
               ?assertEqual(error, logi_backend_table:find_backend(test, logi_backend:get_id(Backend2)))
       end},
      {"存在しないIDが指定された場合は単に無視される",
       fun () ->
               Backend = logi_backend:make(backend1, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, ?INFO, Backend),

               ?assertEqual(ok, logi_backend_table:deregister_backend(test, backend2))
       end}
     ]}.

select_backends_test_() ->
    {foreach, local,
     fun () -> test = logi_backend_table:new(test) end,
     fun (_) -> ok = logi_backend_table:delete(test) end,
     [
      {"登録したバックエンドの中から、条件に一致するものを選択できる",
       fun () ->
               %%%% 登録

               %% info以上なら対象
               Condition1 = logi_condition:make(info),
               Backend1 = logi_backend:make(backend1, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, Condition1, Backend1),

               %% warning以上 かつ メタデータ内に`{module, hoge}'が含まれている なら対象
               Condition2 = logi_condition:make({warning, {match, {?MODULE, metadata_member, {module, hoge}}}}),
               Backend2 = logi_backend:make(backend2, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, Condition2, Backend2),

               %% 以下のいずれかなら対象:
               %% - debug以上 かつ メタデータ内に`{module, hoge}'が含まれている
               %% - info以上 かつ メタデータ内に`{module, fuga}'が含まれている
               Condition3 = logi_condition:make([{debug, {match, {?MODULE, metadata_member, {module, hoge}}}},
                                                 {info,  {match, {?MODULE, metadata_member, {module, fuga}}}}]),
               Backend3 = logi_backend:make(backend2, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, Condition3, Backend3),

               Location = logi_location:make(logi, log, 100),
               
               %%%% 選択
               
               %% severity=info, metadata=[]
               ?assertEqual([Backend1],
                            logi_backend_table:select_backends(test, info, Location, [], [])),

               %% severity=warning, metadata=[]
               ?assertEqual([Backend1],
                            logi_backend_table:select_backends(test, warning, Location, [], [])),

               %% severity=warning, metadata=[{module, hoge}]
               ?assertEqual([Backend1, Backend3],
                            logi_backend_table:select_backends(test, warning, Location, [], [{module, hoge}])),

               %% severity=info, metadata=[{module, hoge}]
               ?assertEqual([Backend1, Backend3],
                            logi_backend_table:select_backends(test, info, Location, [], [{module, hoge}])),

               %% severity=verbose, metadata=[{module, hoge}]
               ?assertEqual([Backend3],
                            logi_backend_table:select_backends(test, verbose, Location, [], [{module, hoge}]))
       end},
      {"IDが重複したバックエンドを登録した場合は、後のものの内容が優先される",
       fun () ->
               Location = logi_location:make(logi, log, 100),

               Condition1 = logi_condition:make(debug),
               Backend1 = logi_backend:make(backend1, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, Condition1, Backend1),
               ?assertEqual([Backend1], logi_backend_table:select_backends(test, info, Location, [], [])), % infoでヒットする
                            
               Condition2 = logi_condition:make(alert),
               Backend2 = logi_backend:make(backend1, self(), ?MODULE, []),
               ok = logi_backend_table:register_backend(test, Condition2, Backend2),
               ?assertEqual([], logi_backend_table:select_backends(test, info, Location, [], [])), % infoではヒットしない
               ?assertEqual([Backend2], logi_backend_table:select_backends(test, alert, Location, [], [])) % alertでヒットする
       end}
     ]}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
metadata_member(X, _, _, MetaData) -> lists:member(X, MetaData).
