%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO: doc
-module(logi_io_lib).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         format/3,
         format_error/5
        ]).

-export_size([
              format_option/0
             ]).
%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type format_option() :: {truncate_size, pos_integer()}. % default: 4096

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec format(io:format(), [term()], [format_option()]) -> iodata().
format(Format, Args, Options) ->
    error(badarg, [Format, Args, Options]).

-spec format_error(atom(), term(), logi:stacktrace(), io:format(), [term()]) -> iodata().
format_error(Class, Reason, StackTrace, Format, Args) ->
    error(badarg, [Class, Reason, StackTrace, Format, Args]).
