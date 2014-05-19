%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(logi_backend_table_tests).

-include_lib("eunit/include/eunit.hrl").
-include("logi.hrl").

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
    [
     %% {"新規バックエンドを登録できる",
     %%  fun () ->
     %%          Backend =
     %%              #logi_backend{
     %%                 id        = backend1,
     %%                 ref       = dummy_backend_process_name,
     %%                 module    = dummy_backend_module,
     %%                 options   = [],
     %%                 condition = info
     %%                },
              
     %%  end}
    ].
