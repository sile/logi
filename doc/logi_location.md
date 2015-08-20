

# Module logi_location #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

ログ出力の位置情報を保持するためのモジュール.

Copyright (c) 2014 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-line">line()</a> ###


<pre><code>
line() = pos_integer() | 0
</code></pre>

0 indicates 'Unknown Line'



### <a name="type-location">location()</a> ###


__abstract datatype__: `location()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_application-1">get_application/1</a></td><td>アプリケーション名を取得する.</td></tr><tr><td valign="top"><a href="#get_function-1">get_function/1</a></td><td>関数名を取得する.</td></tr><tr><td valign="top"><a href="#get_line-1">get_line/1</a></td><td>行番号を取得する.</td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>モジュール名を取得する.</td></tr><tr><td valign="top"><a href="#get_node-1">get_node/1</a></td><td>ノード名を取得する.</td></tr><tr><td valign="top"><a href="#get_process-1">get_process/1</a></td><td>プロセスIDを取得する.</td></tr><tr><td valign="top"><a href="#guess_application-1">guess_application/1</a></td><td>モジュール名から、それが属するアプリケーションを推測する.</td></tr><tr><td valign="top"><a href="#make-3">make/3</a></td><td>Equivalent to <a href="#make-6"><tt>make(node(), self(), guess_application(Module), Module,
Function, Line)</tt></a>.</td></tr><tr><td valign="top"><a href="#make-6">make/6</a></td><td>位置情報オブジェクトを作成する.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>連想リスト形式に変換する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_application-1"></a>

### get_application/1 ###

<pre><code>
get_application(Logi_location::<a href="#type-location">location()</a>) -&gt; atom()
</code></pre>
<br />

アプリケーション名を取得する

<a name="get_function-1"></a>

### get_function/1 ###

<pre><code>
get_function(Logi_location::<a href="#type-location">location()</a>) -&gt; atom()
</code></pre>
<br />

関数名を取得する

<a name="get_line-1"></a>

### get_line/1 ###

<pre><code>
get_line(Logi_location::<a href="#type-location">location()</a>) -&gt; <a href="#type-line">line()</a>
</code></pre>
<br />

行番号を取得する

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Logi_location::<a href="#type-location">location()</a>) -&gt; module()
</code></pre>
<br />

モジュール名を取得する

<a name="get_node-1"></a>

### get_node/1 ###

<pre><code>
get_node(Logi_location::<a href="#type-location">location()</a>) -&gt; node()
</code></pre>
<br />

ノード名を取得する

<a name="get_process-1"></a>

### get_process/1 ###

<pre><code>
get_process(Logi_location::<a href="#type-location">location()</a>) -&gt; pid()
</code></pre>
<br />

プロセスIDを取得する

<a name="guess_application-1"></a>

### guess_application/1 ###

<pre><code>
guess_application(Module::module()) -&gt; atom() | undefined
</code></pre>
<br />

モジュール名から、それが属するアプリケーションを推測する

<a name="make-3"></a>

### make/3 ###

<pre><code>
make(Module::module(), Function::atom(), Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

Equivalent to [`make(node(), self(), guess_application(Module), Module,Function, Line)`](#make-6).

<a name="make-6"></a>

### make/6 ###

<pre><code>
make(Node::node(), Pid::pid(), Application::atom(), Module::module(), Function::atom(), Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

位置情報オブジェクトを作成する

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Location::<a href="#type-location">location()</a>) -&gt; <a href="ordsets.md#type-ordset">ordsets:ordset</a>(Entry)
</code></pre>

<ul class="definitions"><li><code>Entry = {node, node()} | {process, pid()} | {application, atom()} | {module, module()} | {function, atom()} | {line, <a href="#type-line">line()</a>}</code></li></ul>

連想リスト形式に変換する

