

# Module logi_backend #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


バックエンドモジュールのインタフェース定義 および バックエンドオブジェクト操作関数を提供.
Copyright (c) 2014 Takeru Ohta <phjgt308@gmail.com>


__This module defines the `logi_backend` behaviour.__<br /> Required callback functions: `write/5`.

<a name="types"></a>

## Data Types ##




### <a name="type-backend">backend()</a> ###


__abstract datatype__: `backend()`




### <a name="type-data">data()</a> ###



<pre><code>
data() = term()
</code></pre>





### <a name="type-id">id()</a> ###



<pre><code>
id() = term()
</code></pre>





### <a name="type-process">process()</a> ###



<pre><code>
process() = atom()
</code></pre>





### <a name="type-spec">spec()</a> ###



<pre><code>
spec() = {<a href="#type-process">process()</a>, module(), <a href="#type-data">data()</a>} | {<a href="#type-id">id()</a>, <a href="#type-process">process()</a>, module(), <a href="#type-data">data()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_data-1">get_data/1</a></td><td>バックエンドに紐付く任意データを取得する.</td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td>バックエンドのIDを取得する.</td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>バックエンドのモジュールを取得する.</td></tr><tr><td valign="top"><a href="#get_process-1">get_process/1</a></td><td>バックエンドプロセスを取得する.</td></tr><tr><td valign="top"><a href="#is_backend-1">is_backend/1</a></td><td>引数の値がbackend()型かどうかを判定する.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>spec()をもとにbackend()を生成する.</td></tr><tr><td valign="top"><a href="#make-4">make/4</a></td><td>バックエンドオブジェクトを生成する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_data-1"></a>

### get_data/1 ###


<pre><code>
get_data(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

バックエンドに紐付く任意データを取得する
<a name="get_id-1"></a>

### get_id/1 ###


<pre><code>
get_id(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

バックエンドのIDを取得する
<a name="get_module-1"></a>

### get_module/1 ###


<pre><code>
get_module(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; module()
</code></pre>
<br />

バックエンドのモジュールを取得する
<a name="get_process-1"></a>

### get_process/1 ###


<pre><code>
get_process(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; <a href="#type-process">process()</a>
</code></pre>
<br />

バックエンドプロセスを取得する
<a name="is_backend-1"></a>

### is_backend/1 ###


<pre><code>
is_backend(X::<a href="#type-backend">backend()</a> | term()) -&gt; boolean()
</code></pre>
<br />

引数の値がbackend()型かどうかを判定する
<a name="make-1"></a>

### make/1 ###


<pre><code>
make(Arg::<a href="#type-spec">spec()</a>) -&gt; <a href="#type-backend">backend()</a>
</code></pre>
<br />

spec()をもとにbackend()を生成する
<a name="make-4"></a>

### make/4 ###


<pre><code>
make(Id::<a href="#type-id">id()</a>, Process::<a href="#type-process">process()</a>, Module::module(), Data::<a href="#type-data">data()</a>) -&gt; <a href="#type-backend">backend()</a>
</code></pre>
<br />

バックエンドオブジェクトを生成する
