%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc IO系のライブラリ
%%
%% TODO: 最適化
-module(logi_io_lib).
-compile({parse_transform, logi_transform}).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         format/2, format/3
        ]).

-export_size([
              format_option/0
             ]).
%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(DEFAULT_TRUNCATE_SIZE, 4096).

-type format_option() :: {truncate_size, pos_integer()}. % default: 4096

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv format(Format, Arg, [])
-spec format(io:format(), [term()]) -> iodata().
format(Format, Args) ->
    format(Format, Args, []).

-spec format(io:format(), [term()], [format_option()]) -> iodata().
format(Format, Args, Options) ->
    Result = re:replace(io_lib:format(Format, Args), "[\\n\\r]+", "", [{return, binary}]),
    abbreviate(Result, logi_util_assoc:fetch(truncate_size, Options, ?DEFAULT_TRUNCATE_SIZE), <<"...">>).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec abbreviate(Input::binary(), MaxLength::non_neg_integer(), Ellipsis::binary()) -> binary().
abbreviate(<<Bin/binary>>, MaxLength, <<Ellipsis/binary>>) when is_integer(MaxLength), MaxLength >= 0 ->
    case byte_size(Bin) =< MaxLength of
        true  -> Bin;
        false ->
            EllipsisSize = byte_size(Ellipsis),
            TruncateSize = max(0, MaxLength - EllipsisSize),
            <<(binary:part(Bin, 0, TruncateSize))/binary, Ellipsis/binary>>
    end.
