%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_logger_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a logger",
      fun () ->
              Logger = logi_logger:new([]),
              ?assert(logi_logger:is_logger(Logger))
      end},
     {"Converts to/from a map",
      fun () ->
              Logger = logi_logger:new([]),
              MapForm = logi_logger:to_map(Logger),
              ?assertEqual(#{channel => logi_channel:default_channel(), headers => #{}, metadata => #{}}, MapForm),
              ?assertEqual(Logger, logi_logger:from_map(MapForm))
      end}
    ].

channel_test_() ->
    [
     {"Creates a logger with the default channel",
      fun () ->
              Logger = logi_logger:new([]),
              ?assertEqual(Logger, logi_logger:new([{channel, logi_channel:default_channel()}])),
              ?assertEqual(logi_channel:default_channel(), logi_logger:get_channel(Logger))
      end},
     {"Creates a logger with a non default channel",
      fun () ->
              Channel = test_log,
              ?assertNotEqual(Channel, logi_channel:default_channel()),

              Logger = logi_logger:new([{channel, Channel}]),
              ?assertEqual(Channel, logi_logger:get_channel(Logger))
      end}
    ].

headers_test_() ->
    [
     {"Creates a logger with headers",
      fun () ->
              Logger0 = logi_logger:new([]),
              ?assertEqual(#{}, logi_logger:get_headers(Logger0)),

              Logger1 = logi_logger:new([{headers, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, logi_logger:get_headers(Logger1))
      end},
     {"logi_logger:to_map/1",
      fun () ->
              Logger = logi_logger:new([{headers, #{aaa => 111}}]),
              ?assertMatch(#{headers := #{aaa := 111}}, logi_logger:to_map(Logger))
      end},
     {"Sets headers",
      fun () ->
              Logger0 = logi_logger:new([{headers, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, logi_logger:get_headers(Logger0)),

              Logger1 = logi_logger:set_headers(#{aaa => 123, bbb => 222}, ignore, Logger0),
              ?assertEqual(#{aaa => 111, bbb => 222}, logi_logger:get_headers(Logger1)),

              Logger2 = logi_logger:set_headers(#{aaa => 123, ccc => 333}, overwrite, Logger1),
              ?assertEqual(#{aaa => 123, bbb => 222, ccc => 333}, logi_logger:get_headers(Logger2)),

              Logger3 = logi_logger:set_headers(#{aaa => 111}, supersede, Logger2),
              ?assertEqual(#{aaa => 111}, logi_logger:get_headers(Logger3))
      end},
     {"Deletes headers",
      fun () ->
              Logger0 = logi_logger:new([{headers, #{aaa => 111, ccc => 333}}]),
              ?assertEqual(#{aaa => 111, ccc => 333}, logi_logger:get_headers(Logger0)),

              Logger1 = logi_logger:delete_headers([bbb, ccc], Logger0),
              ?assertEqual(#{aaa => 111}, logi_logger:get_headers(Logger1))
      end}
    ].

metadata_test_() ->
    [
     {"Creates a logger with metadata",
      fun () ->
              Logger0 = logi_logger:new([]),
              ?assertEqual(#{}, logi_logger:get_metadata(Logger0)),

              Logger1 = logi_logger:new([{metadata, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, logi_logger:get_metadata(Logger1))
      end},
     {"logi_logger:to_map/1",
      fun () ->
              Logger = logi_logger:new([{metadata, #{aaa => 111}}]),
              ?assertMatch(#{metadata := #{aaa := 111}}, logi_logger:to_map(Logger))
      end},
     {"Sets metadata",
      fun () ->
              Logger0 = logi_logger:new([{metadata, #{aaa => 111}}]),
              ?assertEqual(#{aaa => 111}, logi_logger:get_metadata(Logger0)),

              Logger1 = logi_logger:set_metadata(#{aaa => 123, bbb => 222}, ignore, Logger0),
              ?assertEqual(#{aaa => 111, bbb => 222}, logi_logger:get_metadata(Logger1)),

              Logger2 = logi_logger:set_metadata(#{aaa => 123, ccc => 333}, overwrite, Logger1),
              ?assertEqual(#{aaa => 123, bbb => 222, ccc => 333}, logi_logger:get_metadata(Logger2)),

              Logger3 = logi_logger:set_metadata(#{aaa => 111}, supersede, Logger2),
              ?assertEqual(#{aaa => 111}, logi_logger:get_metadata(Logger3))
      end},
     {"Deletes metadata",
      fun () ->
              Logger0 = logi_logger:new([{metadata, #{aaa => 111, ccc => 333}}]),
              ?assertEqual(#{aaa => 111, ccc => 333}, logi_logger:get_metadata(Logger0)),

              Logger1 = logi_logger:delete_metadata([bbb, ccc], Logger0),
              ?assertEqual(#{aaa => 111}, logi_logger:get_metadata(Logger1))
      end}
    ].

filter_test_() ->
    [
     {"[DEFAULT] A logger does not have a filter",
      fun () ->
              Logger = logi_logger:new([]),
              ?assertEqual(error, logi_logger:get_filter(Logger))
      end},
     {"Creates a logger with a filter",
      fun () ->
              Filter = logi_builtin_filter_fun:new(fun (_) -> true end),
              Logger = logi_logger:new([{filter, Filter}]),
              ?assertEqual({ok, Filter}, logi_logger:get_filter(Logger))
      end},
     {"logi_logger:to_map/1",
      fun () ->
              Logger0 = logi_logger:new([]),
              ?assertNotMatch(#{filter := _}, logi_logger:to_map(Logger0)),

              Filter = logi_builtin_filter_fun:new(fun (_) -> true end),
              Logger1 = logi_logger:new([{filter, Filter}]),
              ?assertMatch(#{filter := Filter}, logi_logger:to_map(Logger1))
      end}
    ].

next_test_() ->
    [
     {"[DEFAULT] A logger does not have a next logger",
      fun () ->
              Logger = logi_logger:new([]),
              ?assertEqual(error, logi_logger:get_next(Logger))
      end},
     {"Creates a logger with a next logger",
      fun () ->
              Next = logi_logger:new([]),
              Logger = logi_logger:new([{next, Next}]),
              ?assertEqual({ok, Next}, logi_logger:get_next(Logger))
      end},
     {"logi_logger:to_map/1",
      fun () ->
              Logger0 = logi_logger:new([]),
              ?assertNotMatch(#{next := _}, logi_logger:to_map(Logger0)),

              Next = logi_logger:new([]),
              Logger1 = logi_logger:new([{next, Next}]),
              ?assertMatch(#{next := Next}, logi_logger:to_map(Logger1))
      end}
    ].
