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
-export_type([form/0, line_or_anno/0, expr/0, expr_call_remote/0, expr_var/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Records
%%----------------------------------------------------------------------------------------------------------------------
-type form() :: {attribute, line_or_anno(), atom(), term()}
              | {function, line_or_anno(), atom(), non_neg_integer(), [clause()]}
              | erl_parse:abstract_form().

-type clause() :: {clause, line_or_anno(), [term()], [term()], [expr()]}
                | erl_parse:abstract_clause().

-type expr() :: expr_call_remote()
              | expr_var()
              | erl_parse:abstract_expr()
              | term().

-type expr_call_remote() :: {call, line_or_anno(), {remote, line_or_anno(), expr(), expr()}, [expr()]}.
-type expr_var() :: {var, line_or_anno(), atom()}.

-type line_or_anno() :: non_neg_integer() | erl_anno:anno().

-record(location,
        {
          application  :: atom(),
          module       :: module(),
          function     :: atom(),
          line_or_anno :: line_or_anno()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Performs transformations for logi
-spec parse_transform([form()], [compile:option()]) -> [form()].
parse_transform(AbstractForms, Options) ->
    Loc = #location{
             application         = logi_transform_utils:guess_application(AbstractForms, Options),
             module              = logi_transform_utils:get_module(AbstractForms),
             line_or_anno        = 0
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
         {clause, LineOrAnno, Args, Guards, Body} -> {clause, LineOrAnno, Args, Guards, [walk_expr(E, Loc) || E <- Body]};
         _                                        -> Clause
     end || Clause <- Clauses].

-spec walk_expr(expr(), #location{}) -> expr().
walk_expr({call, LineOrAnno, {remote, _, {atom, _, M}, {atom, _, F}}, _} = C0, Loc) ->
    C1 = list_to_tuple(walk_expr_parts(tuple_to_list(C0), Loc)),
    transform_call(M, F, C1, Loc#location{line_or_anno = LineOrAnno});
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
transform_call(logi, Severity0, {_, _, _, Args} = Call, Loc = #location{line_or_anno = LineOrAnno}) ->
    Severity = normalize_severity(Severity0),
    case logi:is_severity(Severity) of
        false -> Call;
        true  ->
            case Args of
                %% For maintaining compatibility with v0.0.12
                [Logger, {string, _, _} = Fmt] ->
                    Opts = {cons, LineOrAnno, {tuple, LineOrAnno, [{atom, LineOrAnno, logger}, Logger]}, {nil, LineOrAnno}},
                    logi_call_expr(Severity, Fmt, {nil, LineOrAnno}, Opts, Loc);
                [Logger, {string, _, _} = Fmt, {nil, _} = Data] ->
                    Opts = {cons, LineOrAnno, {tuple, LineOrAnno, [{atom, LineOrAnno, logger}, Logger]}, {nil, LineOrAnno}},
                    logi_call_expr(Severity, Fmt, Data, Opts, Loc);
                [Logger, {string, _, _} = Fmt, {cons, _, _, _} = Data] ->
                    Opts = {cons, LineOrAnno, {tuple, LineOrAnno, [{atom, LineOrAnno, logger}, Logger]}, {nil, LineOrAnno}},
                    logi_call_expr(Severity, Fmt, Data, Opts, Loc);

                [Fmt]             -> logi_call_expr(Severity, Fmt, {nil, LineOrAnno}, {nil, LineOrAnno}, Loc);
                [Fmt, Data]       -> logi_call_expr(Severity, Fmt, Data,        {nil, LineOrAnno}, Loc);
                [Fmt, Data, Opts] -> logi_call_expr(Severity, Fmt, Data,        Opts,        Loc);
                _                 -> Call
            end
    end;
transform_call(_, _, Call, _Loc) ->
    Call.

-spec logi_location_expr(#location{}) -> expr().
logi_location_expr(Loc = #location{line_or_anno = LineOrAnno}) ->
    logi_transform_utils:make_call_remote(
      LineOrAnno, logi_location, unsafe_new,
      [
       {call, LineOrAnno, {atom, LineOrAnno, self}, []},
       {atom, LineOrAnno, Loc#location.application},
       {atom, LineOrAnno, Loc#location.module},
       {atom, LineOrAnno, Loc#location.function},
       {integer, LineOrAnno, logi_transform_utils:line_or_anno_to_line(LineOrAnno)}
      ]).

-spec logi_call_expr(logi:severity(), expr(), expr(), expr(), #location{}) -> expr().
logi_call_expr(Severity, FormatExpr, DataExpr, OptionsExpr, Loc = #location{line_or_anno = LineOrAnno}) ->
    LocationExpr = logi_location_expr(Loc),
    LoggerVar = logi_transform_utils:make_var(LineOrAnno, "__Logger"),
    ResultVar = logi_transform_utils:make_var(LineOrAnno, "__Result"),
    LogiReadyCall = logi_transform_utils:make_call_remote(
                      LineOrAnno, logi, '_ready', [{atom, LineOrAnno, Severity}, LocationExpr, OptionsExpr]),
    {'case', LineOrAnno, LogiReadyCall,
     [
      %% {Logger, []} -> Logger
      {clause, LineOrAnno, [{tuple, LineOrAnno, [LoggerVar, {nil, LineOrAnno}]}], [],
       [LoggerVar]},

      %% {Logger, Result} -> logi:'_write'(Result, Format, Data), Logger
      {clause, LineOrAnno, [{tuple, LineOrAnno, [LoggerVar, ResultVar]}], [],
       [logi_transform_utils:make_call_remote(LineOrAnno, logi, '_write', [ResultVar, FormatExpr, DataExpr]),
        LoggerVar]}
     ]}.

-spec normalize_severity(atom()) -> atom().
normalize_severity(debug_opt) -> debug;
normalize_severity(verbose_opt) -> debug;
normalize_severity(info_opt) -> info;
normalize_severity(notice_opt) -> notice;
normalize_severity(warning_opt) -> warning;
normalize_severity(Other) -> Other.
