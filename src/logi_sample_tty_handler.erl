-module(logi_sample_tty_handler).

-include("logi.hrl").

-export([format/4, write/2]).

format(_State, Format, Args, _Options) ->
    io_lib:format(Format, Args).

write(dummy, _Msg) ->
    ok;
write(_State, Msg) ->
    io:format([Msg, "\n"]).

    
