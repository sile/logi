%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Utility functions for logi_transform
%% @private
%% @end
-module(logi_transform_utils).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([guess_application/2]).
-export([get_module/1]).
-export([make_var/2]).
-export([make_call_remote/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Retrieves the module name from the abstract syntax tree
-spec get_module([logi_transform:form()]) -> module().
get_module([{attribute, _, module, Module} | _]) -> Module;  % a `module' attribute will always exist
get_module([_                              | T]) -> get_module(T).

%% @doc Guesses the application name from the abstract syntax tree `Froms' and the compiler options `Options'
-spec guess_application([logi_transform:form()], [compile:option()]) -> atom() | undefined.
guess_application(Forms, Options) ->
    OutDir = proplists:get_value(outdir, Options),
    SrcDir = case hd(Forms) of
                 {attribute, _, file, {FilePath, _}} -> filename:dirname(FilePath);
                 _                                   -> undefined
             end,
    find_app_file([Dir || Dir <- [OutDir, SrcDir], Dir =/= undefined]).

%% @doc Makes a abstract term for variable
-spec make_var(logi_transform:line(), string()) -> logi_transform:expr_var().
make_var(Line, Prefix) ->
    Seq = case get({?MODULE, seq}) of
              undefined -> 0;
              Seq0      -> Seq0
          end,
    _ = put({?MODULE, seq}, Seq + 1),
    Name = list_to_atom(Prefix ++ "_line" ++ integer_to_list(Line) ++ "_" ++ integer_to_list(Seq)),
    {var, Line, Name}.

%% @doc Makes a abstract term for external function call
-spec make_call_remote(logi_transform:line(), module(), atom(), [logi_transform:expr()]) -> logi_transform:expr_call_remote().
make_call_remote(Line, Module, Function, ArgsExpr) ->
    {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, ArgsExpr}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec find_app_file([string()]) -> atom() | undefined.
find_app_file([])           -> undefined;
find_app_file([Dir | Dirs]) ->
    case filelib:wildcard(Dir++"/*.{app,app.src}") of
        [File] ->
            case file:consult(File) of
                {ok, [{application, AppName, _}|_]} -> AppName;
                _                                   -> find_app_file(Dirs)
            end;
        _ -> find_app_file(Dirs)
    end.
