%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Appenders management table
%% @private
-module(logi_appender_table).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).
-export([which_appenders/1]).

-export_type([table/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type table() :: ets:tab().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new table for the logger `LoggerId'
-spec new(logi:logger_id()) -> table().
new(LoggerId) ->
    ets:new(LoggerId, [set, protected, {read_concurrency, true}, named_table]).

%% @doc Returns a list of existing appenders
-spec which_appenders(table()) -> [logi_appender:id()].
which_appenders(Table) ->
    [Id || {{appender, Id}, _} <- ets:tab2list(Table)].
