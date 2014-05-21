%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_transform).

-export([parse_transform/2]).

%% [before]
%%
%% logi:info("hello: ~p", [world], []).
%%
%% [after]
%%
%% Application = case guess_application(AST) of
%%                 false -> case application:get_application(?MODULE) of
%%                            undefined -> undefined;
%%                            {ok, App} -> App
%%                          end
%%                 {true, App} -> App
%%               end,
%% Functions = guess_function(?LINE, AST),
%% Location = logi_location:make(node(), self(), Application, ?MODULE, Function, ?LINE),
%% logi:log(logi:default_backend_manager(), info, Location, "hello: ~p", [world], [])
%%

-record(location,
        {
          application :: atom(),
          module      :: module(),
          function    :: atom(),
          line        :: pos_integer()
        }).

%% @private
parse_transform(AST, Options) ->
    Application = guess_application(proplists:get_value(outdir, Options), hd(AST)),
    Location = #location{application = Application},
    walk_ast([], AST, Location).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
walk_ast(Acc, [], _Location) ->
    lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, Module} = H | T], Location) ->
    walk_ast([H | Acc], T, Location#location{module = Module});
walk_ast(Acc, [{function, Line, Name, Arity, Clauses} | T], Location) ->
    Acc2 = [{function, Line, Name, Arity, walk_clauses([], Clauses, Location#location{function = Name})} | Acc],
    walk_ast(Acc2, T, Location);
walk_ast(Acc, [H | T], Location) ->
    walk_ast([H | Acc], T, Location).

walk_clauses(Acc, [], _Location) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Args, Guards, Body} | T], Location) ->
    walk_clauses([{clause, Line, Args, Guards, walk_body([], Body, Location)} | Acc], T, Location).

walk_body(Acc, [], _Location) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T], Location) ->
    walk_body([transform_statement(H,Location) | Acc], T, Location).

expand_severity(Severity) ->
    case lists:member(Severity, logi:log_levels()) of
        true  -> {Severity, false};
        false ->
            Bin = atom_to_binary(Severity, utf8),
            PrefixSize = max(0, byte_size(Bin) - byte_size(<<"_opt">>)),
            case Bin of
                <<Prefix:PrefixSize/binary, "_opt">> ->
                    Severity2 = binary_to_atom(Prefix, utf8),
                    case lists:member(Severity2, logi:log_levels()) of
                        true  -> {Severity2, true};
                        false -> false
                    end;
                _ ->
                    false
            end
    end.

default_manager(Line) ->
    {atom, Line, logi:default_backend_manager()}.

transform_statement({call, Line, {remote, _Line1, {atom, _Line2, logi}, {atom, _Line3, Severity0}}, Args} = Stmt, Location) ->
    Location2 = Location#location{line = Line},
    case {expand_severity(Severity0), Args} of
        {false, _}                        -> Stmt;
        {{Severity, false}, [_]}          -> transform_log_statement(default_manager(Line), Severity, Args ++ [{nil, Line}, {nil, Line}], Location2);
        {{Severity, false}, [_, _]}       -> transform_log_statement(default_manager(Line), Severity, Args ++ [{nil, Line}], Location2);
        {{Severity, false}, [_, _, _]}    -> transform_log_statement(hd(Args), Severity, tl(Args) ++ [{nil, Line}], Location2);
        {{Severity, true},  [Msg, Opts]}  -> transform_log_statement(default_manager(Line), Severity, [Msg, {nil, Line}, Opts], Location2);
        {{Severity, true},  [_, _, _]}    -> transform_log_statement(default_manager(Line), Severity, Args, Location2);
        {{Severity, true},  [_, _, _, _]} -> transform_log_statement(hd(Args), Severity, tl(Args), Location2);
        _                                 -> Stmt
    end;
transform_statement(Stmt, Location) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt), Location));
transform_statement(Stmt, Location) when is_list(Stmt) ->
    [transform_statement(S, Location) || S <- Stmt];
transform_statement(Stmt, _Location) ->
    Stmt.

transform_log_statement(ManagerAst, Severity, ArgsAst, Location) ->
    #location{line = Line} = Location,
    LocationAst
        = {tuple, Line,
           [
            {atom, Line, logi_location},
            {call, Line, {atom, Line, node}, []},
            {call, Line, {atom, Line, self}, []},
            {atom, Line, Location#location.application},
            {atom, Line, Location#location.module},
            {atom, Line, Location#location.function},
            {integer, Line, Line}
           ]},
    {call, Line, {remote, Line, {atom, Line, logi}, {atom, Line, log}},
     [
      ManagerAst,
      {atom, Line, Severity},
      LocationAst | ArgsAst
     ]}.

guess_application(Dirname, Attr) when Dirname /= undefined ->
    case find_app_file(Dirname) of
        undefined -> guess_application(undefined, Attr);
        Appname   -> Appname
    end;
guess_application(undefined, {attribute, _, file, {Filename, _}}) ->
    Dir = filename:dirname(Filename),
    find_app_file(Dir);
guess_application(_, _) ->
    undefined.

find_app_file(Dir) ->
    case filelib:wildcard(Dir++"/*.{app,app.src}") of
        []     -> undefined;
        [File] ->
            case file:consult(File) of
                {ok, [{application, Appname, _Attributes}|_]} -> Appname;
                _                                             -> undefined
            end;
        _ -> undefined
    end.
