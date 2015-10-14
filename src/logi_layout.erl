%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Message Layout Behaviour
%%
%% This module defines the standard interface to format log messages issued by `logi' functions
%% (e.g. {@link logi:info/3}, {@link logi:warning/3}, etc).
%%
%% <pre lang="erlang">
%% %%%
%% %%% Example
%% %%%
%% > Context = logi_context:new(sample_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}).
%% > FormatFun = fun (_, Format, Data) -> io_lib:format("EXAMPLE: " ++ Format, Data) end.
%% > Layout = logi_builtin_layout_fun:new(FormatFun).
%% > lists:flatten(logi_layout:format(Context, "Hello ~s", ["World"], Layout)).
%% "EXAMPLE: Hello World"
%% </pre>
-module(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_layout/1]).
-export([get_module/1, get_extra_data/1]).
-export([format/4]).

-export_type([layout/0, layout/1]).
-export_type([data/0]).
-export_type([callback_module/0]).
-export_type([extra_data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback format(logi_context:context(), io:format(), data(), extra_data()) -> iodata().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type layout() :: layout(extra_data()).
%% An instance of `logi_layout' behaviour implementation module.

-opaque layout(ExtraData) :: {callback_module(), ExtraData}
                           | callback_module().
%% A specialized type of `layout/0'.
%% This may be useful for modules which want to annotate their own `ExtraData' type.

-type callback_module() :: module().
%% A module that implements the `logi_layout' behaviour.

-type extra_data() :: term().
%% The value of the fourth arguemnt of the `format/4' callback function.
%%
%% If the `layout()' does not have an explicit `extra_data()', `undefined' will be passed instead.

-type data() :: [term()].
%% A data which is subject to format

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Module, undefined)
-spec new(callback_module()) -> layout().
new(Module) -> new(Module, undefined).

%% @doc Creates a new layout instance
-spec new(callback_module(), ExtraData) -> layout(ExtraData) when ExtraData :: extra_data().
new(Module, ExtraData) ->
    _ = is_layout(Module) orelse error(badarg, [Module, ExtraData]),
    case ExtraData of
        undefined -> Module;
        _         -> {Module, ExtraData}
    end.

%% @doc Returns `true' if `X' is a layout, `false' otherwise
-spec is_layout(X :: (layout() | term())) -> boolean().
is_layout({Module, _}) -> is_atom(Module) andalso is_layout(Module);
is_layout(Module)      -> is_atom(Module) andalso logi_utils:function_exported(Module, format, 4).

%% @doc Gets the module of `Layout'
-spec get_module(Layout :: layout()) -> callback_module().
get_module(Module) when is_atom(Module) -> Module;
get_module({Module, _})                 -> Module.

%% @doc Gets the extra data of `Layout'
-spec get_extra_data(Layout :: layout()) -> extra_data().
get_extra_data(Module) when is_atom(Module) -> undefined;
get_extra_data({_, ExtraData})              -> ExtraData.

%% @doc Returns an `iodata()' which represents `Data' formatted by `Layout' in accordance with `Format' and `Context'
-spec format(logi_context:context(), io:format(), data(), Layout :: layout()) -> iodata().
format(Context, Format, Data, {Module, Extra}) -> Module:format(Context, Format, Data, Extra);
format(Context, Format, Data, Module)          -> Module:format(Context, Format, Data, undefined).
