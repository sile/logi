%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A parse_transform module for logi
%%
%% This module is used to provide following information automatically to log messages (e.g. the messages produced by {@link logi:info/2}):
%% - Application Name
%% - Module Name
%% - Function Name
%% - Line Number
%%
%% The above functionality will be enabled if the option `{parse_transform, logi_transform}' is passed to the compiler.
%%
%% Reference documentations for parse_transform:
%% - http://www.erlang.org/doc/man/erl_id_trans.html
%% - http://www.erlang.org/doc/apps/erts/absform.html
%%
%% @see logi:log/4
%% @see logi_location:guess_location/0
%% @end
-module(logi_transform).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([parse_transform/2]).
-export_type([form/0, line/0, expr/0, expr_call_remote/0, expr_var/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Records
%%----------------------------------------------------------------------------------------------------------------------
-type form() :: {attribute, line(), atom(), term()}
              | {function, line(), atom(), non_neg_integer(), [clause()]}
              | erl_parse:abstract_form().

-type clause() :: {clause, line(), [term()], [term()], [expr()]}
                | erl_parse:abstract_clause().

-type expr() :: expr_call_remote()
              | expr_var()
              | erl_parse:abstract_expr()
              | term().

-type expr_call_remote() :: {call, line(), {remote, line(), expr(), expr()}, [expr()]}.
-type expr_var() :: {var, line(), atom()}.

-type line() :: non_neg_integer().

-record(location,
        {
          application :: atom(),
          module      :: module(),
          function    :: atom(),
          line        :: line()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Performs transformations for logi
-spec parse_transform([form()], [compile:option()]) -> [form()].
parse_transform(AbstractForms, Options) ->
    Loc = #location{
             application = logi_transform_utils:guess_application(AbstractForms, Options),
             module      = logi_transform_utils:get_module(AbstractForms),
             line        = 0
            },
    walk_forms(AbstractForms, Loc).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec walk_forms([form()], #location{}) -> [form()].
walk_forms(Forms, Loc) ->
    [case Form of
         {function, _, Name, _, Clauses} -> setelement(5, Form, walk_clauses(Clauses, Loc#location{function = Name}));
         _                               -> Form
     end || Form <- Forms].

-spec walk_clauses([clause()], #location{}) -> [clause()].
walk_clauses(Clauses, Loc) ->
    [case Clause of
         {clause, Line, Args, Guards, Body} -> {clause, Line, Args, Guards, [walk_expr(E, Loc) || E <- Body]};
         _                                  -> Clause
     end || Clause <- Clauses].

-spec walk_expr(expr(), #location{}) -> expr().
walk_expr({call, Line, {remote, _, {atom, _, M}, {atom, _, F}}, _} = C0, Loc) ->
    C1 = list_to_tuple(walk_expr_parts(tuple_to_list(C0), Loc)),
    transform_call(M, F, C1, Loc#location{line = Line});
walk_expr(Expr, Loc) when is_tuple(Expr) ->
    list_to_tuple(walk_expr_parts(tuple_to_list(Expr), Loc));
walk_expr(Expr, Loc) when is_list(Expr) ->
    [walk_expr(E, Loc) || E <- Expr];
walk_expr(Expr, _Loc) ->
    Expr.

-spec walk_expr_parts([term()], #location{}) -> [term()].
walk_expr_parts(Parts, Loc) ->
    [walk_expr(MaybeExpr, Loc) || MaybeExpr <- Parts].

-spec transform_call(module(), atom(), expr_call_remote(), #location{}) -> expr().
transform_call(logi_location, guess_location, _, Loc) ->
    logi_location_expr(Loc);
transform_call(logi, Severity0, {_, _, _, Args} = Call, Loc = #location{line = Line}) ->
    Severity = normalize_severity(Severity0),
    case logi:is_severity(Severity) of
        false -> Call;
        true  ->
            case Args of
                [Fmt]             -> logi_call_expr(Severity, Fmt, {nil, Line}, {nil, Line}, Loc);
                [Fmt, Data]       -> logi_call_expr(Severity, Fmt, Data,        {nil, Line}, Loc);
                [Fmt, Data, Opts] -> logi_call_expr(Severity, Fmt, Data,        Opts,        Loc);
                _                 -> Call
            end
    end;
transform_call(_, _, Call, _Loc) ->
    Call.

-spec logi_location_expr(#location{}) -> expr().
logi_location_expr(Loc = #location{line = Line}) ->
    logi_transform_utils:make_call_remote(
      Line, logi_location, unsafe_new,
      [
       {call, Line, {atom, Line, self}, []},
       {atom, Line, Loc#location.application},
       {atom, Line, Loc#location.module},
       {atom, Line, Loc#location.function},
       {integer, Line, Line}
      ]).

-spec logi_call_expr(logi:severity(), expr(), expr(), expr(), #location{}) -> expr().
logi_call_expr(Severity, FormatExpr, DataExpr, OptionsExpr, Loc = #location{line = Line}) ->
    LocationExpr = logi_location_expr(Loc),
    LoggerVar = logi_transform_utils:make_var(Line, "__Logger"),
    ResultVar = logi_transform_utils:make_var(Line, "__Result"),
    LogiReadyCall = logi_transform_utils:make_call_remote(
                      Line, logi, '_ready', [{atom, Line, Severity}, LocationExpr, OptionsExpr]),
    {'case', Line, LogiReadyCall,
     [
      %% {Logger, []} -> Logger
      {clause, Line, [{tuple, Line, [LoggerVar, {nil, Line}]}], [],
       [LoggerVar]},

      %% {Logger, Result} -> logi:'_write'(Result, Format, Data), Logger
      {clause, Line, [{tuple, Line, [LoggerVar, ResultVar]}], [],
       [logi_transform_utils:make_call_remote(Line, logi, '_write', [ResultVar, FormatExpr, DataExpr]),
        LoggerVar]}
     ]}.

-spec normalize_severity(atom()) -> atom().
normalize_severity(debug_opt) -> debug;
normalize_severity(verbose_opt) -> debug;
normalize_severity(info_opt) -> info;
normalize_severity(notice_opt) -> notice;
normalize_severity(warning_opt) -> warning;
normalize_severity(Other) -> Other.
