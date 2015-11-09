

# Module logi_transform #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A parse_transform module for logi.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__See also:__ [logi:log/4](logi.md#log-4), [logi_location:guess_location/0](logi_location.md#guess_location-0).

<a name="description"></a>

## Description ##

This module is used to provide following information automatically to log messages (e.g. the messages produced by [`logi:info/2`](logi.md#info-2)):
- Application Name
- Module Name
- Function Name
- Line Number

The above functionality will be enabled if the option `{parse_transform, logi_transform}` is passed to the compiler.

Reference documentations for parse_transform:
- http://www.erlang.org/doc/man/erl_id_trans.html
- http://www.erlang.org/doc/apps/erts/absform.html

<a name="types"></a>

## Data Types ##




### <a name="type-clause">clause()</a> ###


<pre><code>
clause() = {clause, <a href="#type-line">line()</a>, [term()], [term()], [<a href="#type-expr">expr()</a>]} | <a href="erl_parse.md#type-abstract_clause">erl_parse:abstract_clause()</a>
</code></pre>




### <a name="type-expr">expr()</a> ###


<pre><code>
expr() = <a href="#type-expr_call_remote">expr_call_remote()</a> | <a href="#type-expr_var">expr_var()</a> | <a href="erl_parse.md#type-abstract_expr">erl_parse:abstract_expr()</a>
</code></pre>




### <a name="type-expr_call_remote">expr_call_remote()</a> ###


<pre><code>
expr_call_remote() = {call, <a href="#type-line">line()</a>, {remote, <a href="#type-line">line()</a>, <a href="#type-expr">expr()</a>, <a href="#type-expr">expr()</a>}, [<a href="#type-expr">expr()</a>]}
</code></pre>




### <a name="type-expr_var">expr_var()</a> ###


<pre><code>
expr_var() = {var, <a href="#type-line">line()</a>, atom()}
</code></pre>




### <a name="type-form">form()</a> ###


<pre><code>
form() = {attribute, <a href="#type-line">line()</a>, atom(), term()} | {function, <a href="#type-line">line()</a>, atom(), non_neg_integer(), [<a href="#type-clause">clause()</a>]} | <a href="erl_parse.md#type-abstract_form">erl_parse:abstract_form()</a>
</code></pre>




### <a name="type-line">line()</a> ###


<pre><code>
line() = non_neg_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td>Performs transformations for logi.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_transform-2"></a>

### parse_transform/2 ###

<pre><code>
parse_transform(AbstractForms::[<a href="#type-form">form()</a>], Options::[<a href="compile.md#type-option">compile:option()</a>]) -&gt; [<a href="#type-form">form()</a>]
</code></pre>
<br />

Performs transformations for logi

