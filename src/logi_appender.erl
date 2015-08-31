%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc An appender object
-module(logi_appender).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/2]).
-export([get_module/1, get_extra_data/1]).

-export_type([appender/0]).
-export_type([callback_module/0]).
-export_type([extra_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback append(logi_context:context(), io:format(), [term()], extra_data()) -> any().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque appender() :: {callback_module(), extra_data()}.
-type callback_module() :: module().
-type extra_data() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Makes a new appender
-spec make(callback_module(), extra_data()) -> appender().
make(Module, ExtraData) when is_atom(Module) -> {Module, ExtraData};
make(Module, ExtraData)                      -> error(badarg, [Module, ExtraData]).

%% @doc Gets the module of the appender
-spec get_module(appender()) -> callback_module().
get_module({Module, _}) -> Module.

%% @doc Gets the extra data of the appender
-spec get_extra_data(appender()) -> extra_data().
get_extra_data({_, ExtraData}) -> ExtraData.
