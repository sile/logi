%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_transform_tests).

-compile([{parse_transform, logi_transform}]).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(assertLog(Format, Data, AssertContextFun),
        (fun () ->
             receive
                 {'LOGI_MSG', _, __Context, Format, Data, _} -> AssertContextFun(__Context)
             after 25 ->
                     ?assert(timeout)
             end
         end)()).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
log_test_() ->
    InstallProcessSinkOpt =
        fun (Severity, Optins) ->
                {ok, _} = logi_builtin_sink_process:install(Severity, self(), [{if_exists, supersede} | Optins]),
                ok
        end,
    InstallProcessSink = fun (Severity) -> InstallProcessSinkOpt(Severity, []) end,
    {foreach,
     fun () ->
             {ok, Apps} = application:ensure_all_started(logi),
             application:set_env(logi, warnings_as_errors, true),
             application:set_env(logi, warn_no_parse_transform, true),
             Apps
     end,
     fun (Apps) ->
             logi:erase(),
             lists:foreach(fun application:stop/1, Apps)
     end,
     [
      {"`logi:Severity/Arity` is transformed",
       fun () ->
               InstallProcessSink(info),

               %% [ERROR] function call
               ?assertError(_, apply(logi, info, ["hello world"])),

               %% [NO ERROR] transformed call
               Logger = logi:info("hello world"),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(info, logi_context:get_severity(C)) end),
               ?assert(logi:is_logger(Logger))
       end},
      {"`Data` arugment will not be evaluated if it is unnecessary",
       fun () ->
               InstallProcessSink(info),
               ?assert(logi:is_logger(logi:debug("hello world: ~p", [error(unevaluated)]))),
               ?assertError(evaluated, logi:info("hello world: ~p", [error(evaluated)]))
       end},
      {"`logi_location:guess_location/0` is transformed",
       fun () ->
               ?assertError(_, apply(logi_location, guess_location, [])), % => error (function call)
               Location = logi_location:guess_location(),                 % => ok (transformed)
               ?assert(logi_location:is_location(Location))
       end},
      {"`logi:log/4` is not transformed",
       fun () ->
               InstallProcessSink(info),

               %% [ERROR] Calls `logi:log/4` with default options
               ?assertError(_, logi:log(info, "hello world", [], [])),

               %% [NO ERROR] Calls `logi:log/4` with a explicit `location` option
               Location = logi_location:guess_location(),
               logi:log(info, "hello world", [], [{location, Location}]),
               ?assertLog("hello world", [], fun (C) -> ?assertEqual(Location, logi_context:get_location(C)) end),

               %% [NO ERROR] Allows warnings
               application:set_env(logi, warnings_as_errors, false),
               logi:log(info, "hello world", [], []),
               ?assertLog("hello world", [], fun (C) -> ?assertNotEqual(Location, logi_context:get_location(C)) end)
       end}
     ]}.
