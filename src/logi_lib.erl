%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc
-module(logi_lib).

-include("logi.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         get_default_backend_manager/0,
         get_log_levels/0,
         is_log_level/1
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Function
%%------------------------------------------------------------------------------------------------------------------------
-spec get_default_backend_manager() -> logi:backend_manager_id().
get_default_backend_manager() -> ?LOGI_DEFAULT_BACKEND_MANAGER.

-spec get_log_levels() -> [logi:log_level()].
get_log_levels() -> [debug, verbose, info, notice, warning, error, critical, alert, emergency].

-spec is_log_level(logi:log_level()|term()) -> boolean().
is_log_level(Level) -> lists:member(Level, get_log_levels()).
