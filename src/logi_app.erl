%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc application module
%% @private
-module(logi_app).

-behaviour(application).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([start/2, stop/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
start(_StartType, _StartArgs) ->
    SupResult = logi_sup:start_link(),
    case SupResult of
        {ok, _} ->
            case logi:start_logger(logi:default_logger()) of
                {ok, _}         -> SupResult;
                {error, Reason} -> {error, {cannot_start_default_logger, Reason}}
            end;
        Other -> Other
    end.

%% @private
stop(_State) ->
    ok.
