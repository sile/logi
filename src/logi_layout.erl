%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).
-export([is_layout/1]).

-export_type([layout/0]).
-export_type([extra_arg/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback format(logi_context:context(), io:format(), [term()], extra_arg()) -> iodata().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type layout() :: module() | {module(), extra_arg()}.
-type extra_arg() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec format(logi_context:context(), io:format(), [term()], layout()) -> iodata().
format(Context, Format, FormatArgs, {Module, Extra}) -> Module:format(Context, Format, FormatArgs, Extra);
format(Context, Format, FormatArgs, Module)          -> Module:format(Context, Format, FormatArgs, undefined).

-spec is_layout(layout() | term()) -> boolean().
is_layout({Module, _}) ->
    is_layout(Module);
is_layout(Module) when is_atom(Module) ->
    _ = code:load_file(Module),
    erlang:function_exported(Module, format, 4);
is_layout(_) ->
    false.
