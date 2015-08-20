

# Module logi_transform #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

logi用のparse_transformモジュール.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##

[`logi:location/0`](logi.md#location-0)や[`logi:info/2`](logi.md#info-2)等の呼び出しに自動で以下の位置情報を追加するために使用される:
- アプリケーション名
- モジュール名
- 関数名
- 行番号

対象モジュールのコンパイルオプションに`{parse_transform, logi_transform}`を追加することで、この機能が有効となる。

`logi_transform`を有効にせずに`logi`を使用することは可能だが、有益な情報がログから失われてしまうので推奨はされない。

parse_transformに関しては以下も参考となる:
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_transform-2"></a>

### parse_transform/2 ###

<pre><code>
parse_transform(AbstractForms::[<a href="#type-form">form()</a>], Options::[<a href="compile.md#type-option">compile:option()</a>]) -&gt; [<a href="#type-form">form()</a>]
</code></pre>
<br />

