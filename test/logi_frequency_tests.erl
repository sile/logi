%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
-module(logi_frequency_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
make_test_() ->
    {"Creates default controller",
     fun () ->
             _ = logi_frequency:make(),
             ?assert(true)
     end}.
