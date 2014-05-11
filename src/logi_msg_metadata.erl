%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_msg_metadata).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([load_metadata/1,
         fold/3]).

-export_type([metadata/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(PROCESS_DICTIONARY_KEY, '__LOGI_MSG_METADATA__').

-define(METADATA, ?MODULE).

-record(?METADATA,
        {
          entries = [] :: orddict:orddict() % logi:metadata_key() => logi:metadata_value()
        }).

-opaque metadata() :: #?METADATA{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-spec load_metadata([logi:metadata_entry()]) -> metadata().
load_metadata(PriorEntries) ->
    MetaData = load_metadata(),
    MetaData#?METADATA{entries = lists:ukeymerge(1, lists:ukeysort(1, PriorEntries), MetaData#?METADATA.entries)}.

%% TODO: doc
-spec fold(Fun, Init, metadata()) -> Result when
      Fun     :: fun ((logi:metadata_entry(), Acc) -> Acc),
      Init    :: Acc,
      Result  :: Acc.
fold(Fun, Init, MetaData) ->
    lists:foldl(Fun, Init, MetaData#?METADATA.entries).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec load_metadata() -> metadata().
load_metadata() ->
    case get(?PROCESS_DICTIONARY_KEY) of
        undefined -> #?METADATA{};
        MetaData  -> MetaData
    end.
