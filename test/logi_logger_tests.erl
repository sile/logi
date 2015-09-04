%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_logger_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    Channel = test_channel,
    [
     {"Creates a logger",
      fun () ->
              Logger = logi_logger:new(Channel),
              ?assert(logi_logger:is_logger(Logger))
      end},
     {"Converts to/from a map",
      fun () ->
              Logger = logi_logger:new(Channel),
              ?assertEqual(Logger, logi_logger:from_map(logi_logger:to_map(Logger)))
      end}
    ].

headers_test_() ->
    Channel = test_channel,
    Get = fun (Logger) -> maps:get(headers, logi_logger:to_map(Logger), #{}) end,
    [
     {"Creates a logger with headers",
      fun () ->
              Logger0 = logi_logger:new(Channel),
              ?assertEqual(#{}, Get(Logger0)),

              Logger1 = logi_logger:new(Channel, [{headers, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, Get(Logger1))
      end},
     {"Sets headers",
      fun () ->
              Logger0 = logi_logger:new(Channel, [{headers, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, Get(Logger0)),

              Logger1 = logi_logger:set_headers(#{aaa => 123, bbb => 222}, ignore, Logger0),
              ?assertEqual(#{aaa => 111, bbb => 222}, Get(Logger1)),

              Logger2 = logi_logger:set_headers(#{aaa => 123, ccc => 333}, overwrite, Logger1),
              ?assertEqual(#{aaa => 123, bbb => 222, ccc => 333}, Get(Logger2)),

              Logger3 = logi_logger:set_headers(#{aaa => 111}, supersede, Logger2),
              ?assertEqual(#{aaa => 111}, Get(Logger3))
      end},
     {"Deletes headers",
      fun () ->
              Logger0 = logi_logger:new(Channel, [{headers, #{aaa => 111, ccc => 333}}]),
              ?assertEqual(#{aaa => 111, ccc => 333}, Get(Logger0)),

              Logger1 = logi_logger:delete_headers([bbb, ccc], Logger0),
              ?assertEqual(#{aaa => 111}, Get(Logger1))
      end}
    ].

metadata_test_() ->
    Channel = test_channel,
    Get = fun (Logger) -> maps:get(metadata, logi_logger:to_map(Logger), #{}) end,
    [
     {"Creates a logger with metadata",
      fun () ->
              Logger0 = logi_logger:new(Channel),
              ?assertEqual(#{}, Get(Logger0)),

              Logger1 = logi_logger:new(Channel, [{metadata, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, Get(Logger1))
      end},
     {"Sets metadata",
      fun () ->
              Logger0 = logi_logger:new(Channel, [{metadata, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, Get(Logger0)),

              Logger1 = logi_logger:set_metadata(#{aaa => 123, bbb => 222}, ignore, Logger0),
              ?assertEqual(#{aaa => 111, bbb => 222}, Get(Logger1)),

              Logger2 = logi_logger:set_metadata(#{aaa => 123, ccc => 333}, overwrite, Logger1),
              ?assertEqual(#{aaa => 123, bbb => 222, ccc => 333}, Get(Logger2)),

              Logger3 = logi_logger:set_metadata(#{aaa => 111}, supersede, Logger2),
              ?assertEqual(#{aaa => 111}, Get(Logger3))
      end},
     {"Deletes metadata",
      fun () ->
              Logger0 = logi_logger:new(Channel, [{metadata, #{aaa => 111, ccc => 333}}]),
              ?assertEqual(#{aaa => 111, ccc => 333}, Get(Logger0)),

              Logger1 = logi_logger:delete_metadata([bbb, ccc], Logger0),
              ?assertEqual(#{aaa => 111}, Get(Logger1))
      end}
    ].
