%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_client).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         %% TODO: set_metadata
         %% TODO: save_header, load_header or save_context(metadata and header)
         set_header/3,
         unset_header/3
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type expanded_header_scope() :: process | {scope, term()}.

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
            Scope = get_and_expand_header_scope(Options),
            {HeaderState0, WholeHeaderState} = logi_process_dictionary:get_header(ManagerRef),
            HeaderState1 = merge_header_entries(Scope, HeaderEntries, HeaderState0),
            logi_process_dictionary:set_header(ManagerRef, HeaderState1, WholeHeaderState)
    end.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec validate_header_entries([logi:header_entry()] | term()) -> boolean().
validate_header_entries([]) ->
    true;
validate_header_entries([{K, _} | Entries]) when is_atom(K) ->
    validate_header_entries(Entries);
validate_header_entries([_X | Entries]) ->
    %% TODO: notice
    false;
validate_header_entries(_X) ->
    %% TODO: notice
    false.

-spec validate_set_header_options([logi:set_header_option()] | term()) -> boolean().
validate_set_header_options([]) ->
    true;
validate_set_header_options([{scope, _ScopeId} | Options]) ->
    validate_set_header_options(Options);
validate_set_header_options([_X | Options]) ->
    %% TODO: notice
    validate_set_header_options(Options);
validate_set_header_options(_X) ->
    %% TODO: notice
    false.

-spec get_and_expand_header_scope([logi:set_header_option()]) -> expanded_header_scope().
get_and_expand_header_scope(Options) ->
    noscope
    case proplists:get_value(scope, Options, process) of
        process     -> process;
        application -> application
                           
