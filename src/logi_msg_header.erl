%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(logi_msg_header).

-include("logi_internal.hrl").
-behaviour(logi_msg_context).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([from_list/1,
         set_entries/3,
         unset_entries/3,
         erase_entries/2,
         get_entries/2]).

%% 'logi_msg_context' callbacks
-export([is_instance/1, empty/0]).

-export_type([header/0,
              scope/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(HEADER, ?MODULE).

-record(?HEADER,
        {
          scope_to_entries = gb_trees:empty() :: gb_trees:tree(scope(), ordsets:ordset(logi:header_entry()))
        }).

-opaque header() :: #?HEADER{}.

-type scope() :: logi:header_scope() | process.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-spec empty() -> header().
empty() ->
    #?HEADER{}.

-spec from_list([{scope(), [logi:header_entry()]}]) -> header().
from_list(ScopeToEntries) when is_list(ScopeToEntries) ->
    ScopeToEntriesTree =
        lists:foldl(
          fun ({Scope, Entries}, Acc) ->
                  true = ?ASSERT_PRED_FUN(is_valid_scope, Scope),
                  true = ?ASSERT_PRED_FUN(is_valid_entries, Entries),
                  case Entries of
                      [] -> Acc;
                      _  -> gb_trees:enter(Scope, lists:ukeysort(1, Entries), Acc)
                  end
          end,
          gb_trees:empty(),
          ScopeToEntries),
    #?HEADER{scope_to_entries = ScopeToEntriesTree}.

-spec is_instance(header() | term()) -> boolean().
is_instance(MaybeHeader) -> is_record(MaybeHeader, ?HEADER).

-spec set_entries(scope(), [logi:header_entry()], header()) -> header().
set_entries(Scope, Entries, Header) ->
    true = ?ASSERT_PRED_FUN(is_valid_scope, Scope),
    true = ?ASSERT_PRED_FUN(is_valid_entries, Entries),

    #?HEADER{scope_to_entries = ScopeToEntries0} = Header,
    ExistingEntries =
        case gb_trees:lookup(Scope, ScopeToEntries0) of
            none                      -> [];
            {value, ExistingEntries0} -> ExistingEntries0
        end,
    MergedEntries = lists:ukeymerge(1, lists:ukeysort(1, Entries), ExistingEntries),
    ScopeToEntries1 = gb_trees:enter(Scope, MergedEntries, ScopeToEntries0),
    Header#?HEADER{scope_to_entries = ScopeToEntries1}.

-spec unset_entries(scope(), [logi:header_entry_key()], header()) -> header().
unset_entries(Scope, EntryKeys, Header) ->
    #?HEADER{scope_to_entries = ScopeToEntries0} = Header,
    ExistingEntries =
        case gb_trees:lookup(Scope, ScopeToEntries0) of
            none                      -> [];
            {value, ExistingEntries0} -> ExistingEntries0
        end,
    DeletedEntries = lists:foldl(fun (Key, Acc) -> lists:keydelete(Key, 1, Acc) end, ExistingEntries, EntryKeys),
    ScopeToEntries1 =
        case DeletedEntries of
            [] -> gb_trees:delete_any(Scope, ScopeToEntries0);
            _  -> gb_trees:update(Scope, DeletedEntries, ScopeToEntries0)
        end,
    Header#?HEADER{scope_to_entries = ScopeToEntries1}.

-spec erase_entries(scope(), header()) -> header().
erase_entries(Scope, Header) ->
    #?HEADER{scope_to_entries = ScopeToEntries0} = Header,
    ScopeToEntries1 = gb_trees:delete_any(Scope, ScopeToEntries0),
    Header#?HEADER{scope_to_entries = ScopeToEntries1}.

-spec get_entries([logi:header_scope()], header()) -> ordsets:ordset(logi:header_entry()).
get_entries(ScopeList, Header) ->
    #?HEADER{scope_to_entries = ScopeToEntries} = Header,
    ProcessEntries =
        case gb_trees:lookup(process, ScopeToEntries) of
            none                     -> [];
            {value, ProcessEntries0} -> ProcessEntries0
        end,
    case {gb_trees:size(ScopeToEntries), ProcessEntries} of
        {0, _}     -> [];             % よくあるケースを最適化(1)
        {1, [_|_]} -> ProcessEntries; % よくあるケースを最適化(2)
        _          ->
            lists:foldr(
              fun (Scope, Acc) ->
                      case gb_trees:lookup(Scope, ScopeToEntries) of
                          none             -> Acc;
                          {value, Entries} -> lists:ukeymerge(1, Entries, Acc)
                      end
              end,
              ProcessEntries,
              ScopeList)
    end.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%------------------------------------------------------------------------------------------------------------------------
-spec is_valid_scope(scope()) -> true;
                    (term())  -> false.
is_valid_scope(process) -> true;
is_valid_scope({_, _})  -> true;
is_valid_scope(_)       -> false.

-spec is_valid_entries([logi:header_entry()]) -> true;
                      (term())                -> false.
is_valid_entries([])              -> true;
is_valid_entries([{_, _} | Rest]) -> is_valid_entries(Rest);
is_valid_entries(_)               -> false.
