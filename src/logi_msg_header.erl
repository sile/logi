%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_msg_header).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         set_header/3,
         unset_header/3,
         get_header/2
        ]).

-export_type([header/0,
              context/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(PROCESS_DICTIONARY_KEY, '__LOGI_MSG_HEADER__').

-define(HEADER, ?MODULE).

-record(?HEADER,
        {
          entries_per_scope = gb_trees:empty() :: gb_trees:tree(scope(), [logi:header_entry()])
        }).

-type scope() :: logi:header_scope() | process.

-opaque header()  :: #?HEADER{}.
-opaque context() :: [{logi:event_manager_ref(), header()}].

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: doc
%% TODO: 全体的に不正な引数でもabortはせずにログ出力して処理は進めるようにする (interval指定付きでwarning)
-spec set_header(logi:event_manager_ref(), [logi:header_entry()], [logi:set_header_option()]) -> ok.
set_header(ManagerRef, HeaderEntries, Options) ->
    case validate_set_header_options(Options) andalso validate_header_entries(HeaderEntries) of
        false -> ok;
        true  ->
            Scope = logi_util_assoc:fetch(scope, Options, process),
            {Header0, Context} = load_header(ManagerRef),
            Header1 = merge_header_entries(Scope, HeaderEntries, Header0),
            save_header(ManagerRef, Header1, Context)
    end.

-spec unset_header(logi:event_manager_ref(), [logi:header_entry_key()], [logi:unset_header_option()]) -> ok.
unset_header(ManagerRef, HeaderEntryKeys, Options) ->
    case validate_unset_header_options(Options) andalso validate_header_entry_keys(HeaderEntryKeys) of
        false -> ok;
        true  ->
            Scope = logi_util_assoc:fetch(scope, Options, process),
            {Header0, Context} = load_header(ManagerRef),
            Header1 = delete_header_entries(Scope, HeaderEntryKeys, Header0),
            save_header(ManagerRef, Header1, Context)
    end.

-spec get_header(logi:event_manager_ref(), [logi:get_header_option()]) -> header().
get_header(ManagerRef, Options) ->
    case validate_get_header_options(Options) of
        false -> ok;
        true  ->
            find_header(ManagerRef)
    end.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec load_context() -> context().
load_context() ->
    case get(?PROCESS_DICTIONARY_KEY) of
        undefined -> [];
        Context   -> Context
    end.

-spec save_context(context()) -> ok.
save_context(Context) ->
    _ = put(?PROCESS_DICTIONARY_KEY, Context),
    ok.

-spec find_header(logi:event_manager_ref()) -> header().
find_header(ManagerRef) ->
    case lists:keyfind(ManagerRef, load_context()) of
        {_, Header} -> Header;
        false       -> #?HEADER{}
    end.

-spec load_header(logi:event_manager_ref()) -> {header(), context()}.
load_header(ManagerRef) ->
    Context0 = load_context(),
    case lists:keytake(ManagerRef, 1, Context0) of
        {value, {_, Header}, Context1} -> {Header, Context1};
        false                          -> {#?HEADER{}, Context0}
    end.

-spec save_header(logi:event_manager_ref(), header(), context()) -> ok.
save_header(ManagerRef, Header, Context) ->
    %% TODO: 前提をドキュメント化
    save_context([{ManagerRef, Header} | Context]).
    
-spec validate_header_entries([logi:header_entry()] | term()) -> boolean().
validate_header_entries([]) ->
    true;
validate_header_entries([{K, _} | Entries]) when is_atom(K) ->
    validate_header_entries(Entries);
validate_header_entries([_X | _Entries]) ->
    %% TODO: notice
    false;
validate_header_entries(_X) ->
    %% TODO: notice
    false.

-spec validate_header_entry_keys([logi:header_entry_key()] | term()) -> boolean().
validate_header_entry_keys([]) ->
    true;
validate_header_entry_keys([K | List]) when is_atom(K) ->
    validate_header_entry_keys(List);
validate_header_entry_keys([_ | _]) ->
    %% TODO: notice
    false;
validate_header_entry_keys(_) ->
    %% TODO: notice
    false.

-spec validate_set_header_options([logi:set_header_option()] | term()) -> boolean().
validate_set_header_options([]) ->
    true;
validate_set_header_options([{scope, Scope} | Options]) ->
    IsValid =
        case Scope of
            {K, V} when is_atom(K) -> true;
            _                      ->
                %% TODO: log
                false
        end,
    IsValid andalso validate_set_header_options(Options);
validate_set_header_options([_X | Options]) ->
    %% TODO: notice
    validate_set_header_options(Options);
validate_set_header_options(_X) ->
    %% TODO: notice
    false.

-spec validate_unset_header_options([logi:unset_header_option()] | term()) -> boolean().
validate_unset_header_options(Options) ->
    validate_set_header_options(Options).

-spec validate_get_header_options([logi:get_header_option()] | term()) -> boolean().
validate_get_header_options([]) ->
    true;
validate_get_header_options([{metadata, Scope} | Options]) ->
    IsValid =
        case Scope of
            {K, V} when is_atom(K) -> true;
            _                      ->
                %% TODO: log
                false
        end,
    IsValid andalso validate_get_header_options(Options);
validate_get_header_options([_X | Options]) ->
    %% TODO: notice
    validate_get_header_options(Options);
validate_get_header_options(_X) ->
    %% TODO: notice
    false.

-spec merge_header_entries(logi:header_scope(), [logi:header_entry()], header()) -> header().
merge_header_entries(Scope, Entries, Header) ->
    #?HEADER{entries_per_scope = Map0} = Header,
    ExistingEntries =
        case gb_trees:lookup(Scope, Map0) of
            none                      -> [];
            {value, ExistingEntries0} -> ExistingEntries0
        end,
    MergedEntries = lists:ukeymerge(1, lists:ukeysort(1, Entries), ExistingEntries),
    Map1 = gb_trees:enter(Scope, MergedEntries, Map0),
    Header#?HEADER{entries_per_scope = Map1}.

-spec delete_header_entries(logi:header_scope(), [logi:header_entry_key()], header()) -> header().
delete_header_entries(Scope, EntryKeys, Header) ->
    #?HEADER{entries_per_scope = Map0} = Header,
    ExistingEntries =
        case gb_trees:lookup(Scope, Map0) of
            none                      -> [];
            {value, ExistingEntries0} -> ExistingEntries0
        end,
    DeletedEntries = lists:foldl(fun (Key, Acc) -> lists:keydelete(Key, 1, Acc) end, ExistingEntries, EntryKeys),
    Map1 =
        case DeletedEntries of
            [] -> gb_trees:delete_any(Scope, Map0);
            _  -> gb_trees:enter(Scope, DeletedEntries, Map0)
        end,
    Header#?HEADER{entries_per_scope = Map1}.
