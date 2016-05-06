%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
%%----------------------------------------------------------
%% Constants
%%----------------------------------------------------------
severity_test_() ->
    [
     {"Available severities",
      fun () ->
              ?assertEqual([emergency, alert, critical, error, warning, notice, info, debug], logi:severities()),
              ?assert(lists:all(fun logi:is_severity/1, logi:severities())),
              ?assertNot(logi:is_severity(trace))
      end},
     {"Severity level",
      fun () ->
              lists:zipwith(
                fun (Level, Severity) ->
                        ?assertEqual(Level, logi:severity_level(Severity))
                end,
                lists:seq(1, length(logi:severities())),
                logi:severities())
      end},
     {"[ERROR] Getting the level of an undefined severity",
      fun () ->
              ?assertError(badarg, logi:severity_level(trace))
      end}
    ].

default_logger_test_() ->
    {"The default logger",
     ?_assertEqual(logi_default_log, logi:default_logger())}.

%%----------------------------------------------------------
%% Logger
%%---------------------------------------------------------
new_test_() ->
    [
     {"Creates a logger instance",
      fun () ->
              Logger = logi:new(),
              ?assert(logi:is_logger(Logger))
      end},
     {"Creates with options",
      fun () ->
              Default = logi_channel:default_channel(),
              New = fun (Options) -> logi:to_map(logi:new(Options)) end,

              %% no option
              ?assertEqual(#{channel => Default, headers => #{}, metadata => #{}}, New([])),

              %% with `channel'
              ?assertEqual(#{channel => test_log, headers => #{}, metadata => #{}}, New([{channel, test_log}])),

              %% with `headers'
              ?assertEqual(#{channel => Default, headers => #{a => b}, metadata => #{}}, New([{headers, #{a => b}}])),

              %% with `metadata'
              ?assertEqual(#{channel => Default, headers => #{}, metadata => #{a => b}}, New([{metadata, #{a => b}}])),

              %% with `filter'
              Filter = logi_builtin_filter_fun:new(fun (_) -> true end),
              ?assertEqual(#{channel => Default, headers => #{}, metadata => #{}, filter => Filter}, New([{filter, Filter}])),

              %% with `next'
              ?assertEqual(#{channel => Default, headers => #{}, metadata => #{}, next => logi:new()}, New([{next, logi:new()}]))
      end},
     {"Converts from/to a map",
      fun () ->
              Logger = logi:new(),
              ?assertEqual(Logger, logi:from_map(logi:to_map(Logger)))
      end}
    ].

process_dictionary_test_() ->
    {foreach,
     fun () -> ok end,
     fun (_) -> logi:erase() end,
     [
      {"Saves a logger to the process dictionary",
       fun () ->
               Logger = logi:new(),
               ?assertEqual(undefined,    logi:save(hoge, Logger)), % save
               ?assertEqual([hoge],       logi:which_loggers()),    % list
               ?assertEqual({ok, Logger}, logi:load(hoge)),         % load
               ?assertEqual(Logger,       logi:erase(hoge)),        % erase
               ?assertEqual([],           logi:which_loggers())     % list
       end},
      {"Saves as the default logger",
       fun () ->
               Logger = logi:new(),
               ?assertEqual(undefined,               logi:save_as_default(Logger)),
               ?assertEqual([logi:default_logger()], logi:which_loggers()),
               ?assertEqual({ok, Logger},            logi:load(logi:default_logger())),
               ?assertEqual({ok, Logger},            logi:load_default())
       end},
      {"Saves another saved logger by a different name",
       fun () ->
               Logger = logi:new(),
               undefined = logi:save(hoge, Logger),

               ?assertEqual(undefined, logi:save(fuga, hoge)),
               ?assertEqual(lists:sort([fuga, hoge]), lists:sort(logi:which_loggers())),
               ?assertEqual(logi:load(hoge), logi:load(fuga))
       end},
      {"Overwrite a saved logger",
       fun () ->
               Logger0 = logi:new([{channel, log_0}]),
               Logger1 = logi:new([{channel, log_1}]),
               ?assertEqual(undefined,     logi:save(hoge, Logger0)), % save
               ?assertEqual(Logger0,       logi:save(hoge, Logger1)), % overwrite
               ?assertEqual({ok, Logger1}, logi:load(hoge))           % load
       end},
      {"Erases all saved loggers",
       fun () ->
               Logger0 = logi:new([{channel, log_0}]),
               Logger1 = logi:new([{channel, log_1}]),
               undefined = logi:save(log_0, Logger0),
               undefined = logi:save(log_1, Logger1),
               ?assertEqual(lists:sort([{log_0, Logger0}, {log_1, Logger1}]), lists:sort(logi:erase())),
               ?assertEqual([], logi:which_loggers())
       end},
      {"Applies logi:to_map/1 to a saved logger",
       fun () ->
               Logger = logi:new(),
               undefined = logi:save(hoge, Logger),
               ?assertEqual(logi:to_map(Logger), logi:to_map(hoge)),

               %% If the specified logger have not been saved, `logi:new/1' will be implicitly called
               ?assertEqual(error, logi:load(unsaved)),
               ?assertEqual(logi:to_map(logi:new([{channel, unsaved}])), logi:to_map(unsaved)),
               ?assertEqual([hoge], logi:which_loggers())
       end},
      {"logi:ensure_to_be_instance/1",
       fun () ->
               ?assertEqual(logi:new([{channel, unsaved}]), logi:ensure_to_be_instance(unsaved)),

               logi:save(saved, logi:new([{channel, hoge}])),
               ?assertEqual(logi:new([{channel, hoge}]), logi:ensure_to_be_instance(saved)),

               ?assertEqual(logi:new([{channel, fuga}]), logi:ensure_to_be_instance(logi:new([{channel, fuga}])))
       end}
     ]}.

headers_test_() ->
    [
     {"Initially no headers exist",
      fun () ->
              Logger = logi:new(),
              ?assertEqual({ok, #{}}, maps:find(headers, logi:to_map(Logger)))
      end},
     {"Creates a logger with headers",
      fun () ->
              Logger = logi:new([{headers, #{a => b, 1 => 2}}]),
              ?assertEqual({ok, #{a => b, 1 => 2}}, maps:find(headers, logi:to_map(Logger)))
      end},
     {"Sets headers of a logger",
      fun () ->
              Logger0 = logi:new(),
              Logger1 = logi:set_headers(#{a => b, 1 => 2}, [{logger, Logger0}]),
              ?assertEqual({ok, #{a => b, 1 => 2}}, maps:find(headers, logi:to_map(Logger1)))
      end},
     {"Sets headers of a saved logger",
      fun () ->
              logi:save(hoge, logi:new()),
              logi:set_headers(#{a => b, 1 => 2}, [{logger, hoge}]),
              ?assertEqual({ok, #{a => b, 1 => 2}}, maps:find(headers, logi:to_map(hoge))),
              logi:erase()
      end},
     {"Sets headers with `if_exists' option",
      fun () ->
              Set =
                  fun (Logger, Headers, Optins) ->
                          maps:get(headers, logi:to_map(logi:set_headers(Headers, [{logger, Logger} | Optins])))
                  end,
              Logger = logi:new([{headers, #{a => b, "x" => "y"}}]),

              %% if_exists=overwrite (default)
              ?assertEqual(#{a => c, "x" => "y", 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [])),
              ?assertEqual(#{a => c, "x" => "y", 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [{if_exists, overwrite}])),

              %% if_exists=ignore
              ?assertEqual(#{a => b, "x" => "y", 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [{if_exists, ignore}])),

              %% if_exists=supersede
              ?assertEqual(#{a => c, 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [{if_exists, supersede}]))
      end},
     {"Sets headers recursively",
      fun () ->
              Logger0 =
                  logi:from_list(
                    [
                     logi:new([{channel, log_0}, {headers, #{a => b}}]),
                     logi:new([{channel, log_1}, {headers, #{1 => 2}}])
                    ]),

              Logger1 = logi:set_headers(#{"x" => "y"}, [{logger, Logger0}]),
              ?assertEqual(#{a => b, "x" => "y"}, maps:get(headers, logi:to_map(Logger1))),
              ?assertEqual(#{1 => 2, "x" => "y"}, maps:get(headers, logi:to_map(maps:get(next, logi:to_map(Logger1)))))
      end},
     {"Deletes headers",
      fun () ->
              Logger0 =
                  logi:from_list(
                    [
                     logi:new([{channel, log_0}, {headers, #{a => b, "x" => "y"}}]),
                     logi:new([{channel, log_1}, {headers, #{1 => 2, "x" => "y"}}])
                    ]),

              Logger1 = logi:delete_headers(["x"], [{logger, Logger0}]),
              ?assertEqual(#{a => b}, maps:get(headers, logi:to_map(Logger1))),
              ?assertEqual(#{1 => 2}, maps:get(headers, logi:to_map(maps:get(next, logi:to_map(Logger1))))),

              %% saved
              logi:save_as_default(Logger0),
              logi:delete_headers(["x"]),
              ?assertEqual(#{a => b}, maps:get(headers, logi:to_map(logi:default_logger()))),
              ?assertEqual(#{1 => 2}, maps:get(headers, logi:to_map(maps:get(next, logi:to_map(logi:default_logger())))))
      end}
    ].

metadata_test_() ->
    [
     {"Initially no metadata exist",
      fun () ->
              Logger = logi:new(),
              ?assertEqual({ok, #{}}, maps:find(metadata, logi:to_map(Logger)))
      end},
     {"Creates a logger with metadata",
      fun () ->
              Logger = logi:new([{metadata, #{a => b, 1 => 2}}]),
              ?assertEqual({ok, #{a => b, 1 => 2}}, maps:find(metadata, logi:to_map(Logger)))
      end},
     {"Sets metadata of a logger",
      fun () ->
              Logger0 = logi:new(),
              Logger1 = logi:set_metadata(#{a => b, 1 => 2}, [{logger, Logger0}]),
              ?assertEqual({ok, #{a => b, 1 => 2}}, maps:find(metadata, logi:to_map(Logger1)))
      end},
     {"Sets metadata of a saved logger",
      fun () ->
              logi:save(hoge, logi:new()),
              logi:set_metadata(#{a => b, 1 => 2}, [{logger, hoge}]),
              ?assertEqual({ok, #{a => b, 1 => 2}}, maps:find(metadata, logi:to_map(hoge))),
              logi:erase()
      end},
     {"Sets metadata with `if_exists' option",
      fun () ->
              Set =
                  fun (Logger, Metadata, Optins) ->
                          maps:get(metadata, logi:to_map(logi:set_metadata(Metadata, [{logger, Logger} | Optins])))
                  end,
              Logger = logi:new([{metadata, #{a => b, "x" => "y"}}]),

              %% if_exists=overwrite (default)
              ?assertEqual(#{a => c, "x" => "y", 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [])),
              ?assertEqual(#{a => c, "x" => "y", 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [{if_exists, overwrite}])),

              %% if_exists=ignore
              ?assertEqual(#{a => b, "x" => "y", 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [{if_exists, ignore}])),

              %% if_exists=supersede
              ?assertEqual(#{a => c, 1 => 2}, Set(Logger, #{a => c, 1 => 2}, [{if_exists, supersede}]))
      end},
     {"Sets metadata recursively",
      fun () ->
              Logger0 =
                  logi:from_list(
                    [
                     logi:new([{channel, log_0}, {metadata, #{a => b}}]),
                     logi:new([{channel, log_1}, {metadata, #{1 => 2}}])
                    ]),

              Logger1 = logi:set_metadata(#{"x" => "y"}, [{logger, Logger0}]),
              ?assertEqual(#{a => b, "x" => "y"}, maps:get(metadata, logi:to_map(Logger1))),
              ?assertEqual(#{1 => 2, "x" => "y"}, maps:get(metadata, logi:to_map(maps:get(next, logi:to_map(Logger1)))))
      end},
     {"Deletes metadata",
      fun () ->
              Logger0 =
                  logi:from_list(
                    [
                     logi:new([{channel, log_0}, {metadata, #{a => b, "x" => "y"}}]),
                     logi:new([{channel, log_1}, {metadata, #{1 => 2, "x" => "y"}}])
                    ]),

              Logger1 = logi:delete_metadata(["x"], [{logger, Logger0}]),
              ?assertEqual(#{a => b}, maps:get(metadata, logi:to_map(Logger1))),
              ?assertEqual(#{1 => 2}, maps:get(metadata, logi:to_map(maps:get(next, logi:to_map(Logger1))))),

              %% saved
              logi:save_as_default(Logger0),
              logi:delete_metadata(["x"]),
              ?assertEqual(#{a => b}, maps:get(metadata, logi:to_map(logi:default_logger()))),
              ?assertEqual(#{1 => 2}, maps:get(metadata, logi:to_map(maps:get(next, logi:to_map(logi:default_logger())))))
      end}
    ].

%%----------------------------------------------------------
%% Logging
%%---------------------------------------------------------
-define(assertLog(Format, Data, AssertContextFun),
        (fun () ->
             receive
                 {'LOGI_MSG', __Context, Format, Data} -> AssertContextFun(__Context)
             after 25 ->
                     ?assert(timeout)
             end
         end)()).

-define(assertNotLog(),
        (fun () ->
                 receive
                     {'LOGI_MSG', _, _, _} -> ?assert(unexpectedly_delivered_log)
                 after 25 ->
                         ?assert(true)
                 end
         end)()).

log_test_() ->
    InstallSinkOpt =
        fun (Severity, Optins) ->
                Id = proplists:get_value(id, Optins),
                Caller = self(),
                WriteFun = fun (Context, Format, Data) -> Caller ! {'LOGI_MSG', Context, Format, Data} end,
                {ok, _} = logi_channel:install_sink(
                            proplists:get_value(channel, Optins, logi_channel:default_channel()),
                            logi_builtin_sink_fun:new(Id, WriteFun), Severity),
                ok
        end,
    InstallSink = fun (Severity) -> InstallSinkOpt(Severity, [{id, test}]) end,
    {foreach,
     fun () ->
             {ok, Apps} = application:ensure_all_started(logi),
             application:set_env(logi, warn_no_parse_transform, false),
             Apps
     end,
     fun (Apps) ->
             logi:erase(),
             lists:foreach(fun application:stop/1, Apps)
     end,
     [
      {"Basic: default options",
       fun () ->
               InstallSink(info),

               logi:log(debug, "Hello ~s", ["World"], []),
               ?assertNotLog(),

               logi:log(info, "Hello ~s", ["World"], []),
               ?assertLog("Hello ~s", ["World"], fun (C) -> ?assertEqual(info, logi_context:get_severity(C)) end),

               logi:log(alert, "Hello ~s", ["World"], []),
               ?assertLog("Hello ~s", ["World"], fun (C) -> ?assertEqual(alert, logi_context:get_severity(C)) end)
       end},
      {"Wrapper: logi:Severity/{1,2,3}",
       fun () ->
               InstallSink(debug),
               lists:foreach(
                 fun (Severity) ->
                         logi:Severity("hello world"),
                         ?assertLog("hello world", [], fun (C) -> ?assertEqual(Severity, logi_context:get_severity(C)) end)
                 end,
                 logi:severities())
       end},
      {"Option: `logger'",
       fun () ->
               ok = logi_channel:create(test_log),
               InstallSinkOpt(info, [{channel, test_log}]),

               logi:info("hello world"),
               ?assertNotLog(),

               logi:info("hello world", [], [{logger, test_log}]),
               ?assertLog("hello world", [], fun (_) -> true end),

               Logger = logi:new([{channel, test_log}]),
               logi:info("hello world", [], [{logger, Logger}]),
               ?assertLog("hello world", [], fun (_) -> true end)
       end},
      {"Option: `headers'",
       fun () ->
               InstallSink(info),

               logi:info("hello world", [], [{headers, #{a => b}}]),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(#{a => b}, logi_context:get_headers(C)) end),

               logi:set_headers(#{a => b}),
               logi:info("hello world"),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(#{a => b}, logi_context:get_headers(C)) end)
       end},
      {"Option: `metadata'",
       fun () ->
               InstallSink(info),

               logi:info("hello world", [], [{metadata, #{a => b}}]),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(#{a => b}, logi_context:get_metadata(C)) end),

               logi:set_metadata(#{a => b}),
               logi:info("hello world"),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(#{a => b}, logi_context:get_metadata(C)) end)
       end},
      {"Option: `location'",
       fun () ->
               InstallSink(info),

               Location = logi_location:guess_location(),
               logi:info("hello world", [], [{location, Location}]),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(Location, logi_context:get_location(C)) end)
       end},
      {"Option: `timestamp'",
       fun () ->
               InstallSink(info),

               Timestamp = {0, 0, 0},
               logi:info("hello world", [], [{timestamp, Timestamp}]),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(Timestamp, logi_context:get_timestamp(C)) end)
       end},
      {"Filter",
       fun () ->
               InstallSink(info),

               Filter =
                   logi_builtin_filter_fun:new(
                     fun (C) ->
                             case logi_context:get_metadata(C) of
                                 #{discard := true} -> false;
                                 _                  -> true
                             end
                     end),
               Logger = logi:new([{filter, Filter}]),
               logi:save_as_default(Logger),

               logi:info("hello world"),
               ?assertLog("hello world", [], fun (_) -> true end),

               logi:info("hello world", [], [{metadata, #{discard => true}}]),
               ?assertNotLog(),

               logi:info("hello world", [], [{metadata, #{discard => false}}]),
               ?assertLog("hello world", [], fun (_) -> true end)
       end},
      {"Multiple sinks",
       fun () ->
               InstallSinkOpt(info, [{id, sink_0}]),
               InstallSinkOpt(info, [{id, sink_1}]),

               logi:info("hello world"),
               ?assertLog("hello world", [], fun (_) -> true end),
               ?assertLog("hello world", [], fun (_) -> true end)
       end},
      {"A crash of a sink is isolated from others",
       fun () ->
               ErroneousWriteFun = fun (_, _, _) -> error(something_wrong) end,
               InstallSinkOpt(info, [{id, sink_0}]),
               {ok, _} = logi_channel:install_sink(logi_builtin_sink_fun:new(sink_1, ErroneousWriteFun), info),
               InstallSinkOpt(info, [{id, sink_2}]),

               logi:info("hello world"),
               ?assertLog("hello world", [], fun (_) -> true end),
               ?assertLog("hello world", [], fun (_) -> true end),
               ?assertNotLog()
       end},
      {"Aggregated logger",
       fun () ->
               InstallSink(info),

               Logger =
                   logi:from_list(
                     [
                      logi:new([{headers, #{id => a}}]),
                      logi:new([{headers, #{id => b}}])
                     ]),
               logi:save_as_default(Logger),

               logi:info("hello world"),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(#{id => a}, logi_context:get_headers(C)) end),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(#{id => b}, logi_context:get_headers(C)) end)
       end},
      {"[ERROR] An empty list is passed to 'logi:fromt_list/1'",
       fun () ->
               ?assertError(badarg, logi:from_list([]))
       end},
      {"logi:to_list/1",
       fun () ->
               Logger0 = logi:new([{headers, #{id => a}}]),
               Logger1 = logi:new([{headers, #{id => b}}]),
               Logger2 = logi:from_list([Logger0 ,Logger1]),
               ?assertEqual([Logger0, Logger1], logi:to_list(Logger2))
       end},
      {"Applies nested logi:from_list/2",
       fun () ->
               Logger0 = logi:new([{headers, #{id => a}}]),
               Logger1 = logi:new([{headers, #{id => b}}]),
               Logger2 = logi:new([{headers, #{id => c}}]),
               Logger3 = logi:new([{headers, #{id => d}}]),
               Logger4 = logi:from_list(
                           [
                            logi:from_list([Logger0, Logger1]),
                            logi:from_list([Logger2, Logger3])
                           ]),
               ?assertEqual([Logger0, Logger1, Logger2, Logger3], logi:to_list(Logger4))
       end}
     ]}.
