%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_msg_metadata).

-include("logi_internal.hrl").
-behaviour(logi_msg_context).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([from_list/1,
         set_entries/2,
         unset_entries/2,
         get_entries/1]).

%% 'logi_msg_context' callbacks
-export([is_instance/1, empty/0]).

-export_type([metadata/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(METADATA, ?MODULE).

-record(?METADATA,
        {
          entries = [] :: orddict:orddict() % logi:metadata_key() => logi:metadata_value()
        }).

-opaque metadata() :: #?METADATA{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec empty() -> metadata().
empty() ->
    #?METADATA{}.

-spec from_list([logi:metadata_entry()]) -> metadata().
from_list(Entries) when is_list(Entries) ->
    true = ?ASSERT_PRED_FUN(is_valid_entries, Entries),
    #?METADATA{entries = orddict:from_list(Entries)}.

-spec is_instance(metadata()) -> true;
                 (term())     -> false.
is_instance(MaybeMetaData) -> is_record(MaybeMetaData, ?METADATA).

-spec set_entries([logi:metadata_entry()], metadata()) -> metadata().
set_entries(Entries, MetaData) ->
    true = ?ASSERT_PRED_FUN(is_valid_entries, Entries),
    #?METADATA{entries = ExistingEntries} = MetaData,
    MergedEntries = lists:ukeymerge(1, lists:ukeysort(1, Entries), ExistingEntries),
    MetaData#?METADATA{entries = MergedEntries}.

-spec unset_entries([logi:metadata_entry_key()], metadata()) -> metadata().
unset_entries(EntryKeys, MetaData) ->
    #?METADATA{entries = ExistingEntries} = MetaData,
    DeletedEntries = lists:foldl(fun (Key, Acc) -> lists:keydelete(Key, 1, Acc) end, ExistingEntries, EntryKeys),
    MetaData#?METADATA{entries = DeletedEntries}.

-spec get_entries(metadata()) -> ordsets:ordset(logi:metadata_entry()).
get_entries(MetaData) ->
    MetaData#?METADATA.entries.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec is_valid_entries([logi:metadata_entry()]) -> true;
                      (term())                  -> false.
is_valid_entries([])              -> true;
is_valid_entries([{_, _} | Rest]) -> is_valid_entries(Rest);
is_valid_entries(_)               -> false.
