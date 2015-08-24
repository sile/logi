

# Module logi_backend #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

backend object.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_backend` behaviour.__<br /> Required callback functions: `write/4`.

<a name="types"></a>

## Data Types ##




### <a name="type-backend">backend()</a> ###


__abstract datatype__: `backend()`




### <a name="type-data">data()</a> ###


<pre><code>
data() = term()
</code></pre>

user defined data



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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_data-1">get_data/1</a></td><td>Gets the backend data.</td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td>Gets the backend ID.</td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the backend module.</td></tr><tr><td valign="top"><a href="#get_process-1">get_process/1</a></td><td>Gets the backend process.</td></tr><tr><td valign="top"><a href="#is_backend-1">is_backend/1</a></td><td>Returns <code>true</code> if <code>Term</code> appears to be a backend, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Makes a new backend object from <code>Spec</code></td></tr><tr><td valign="top"><a href="#make-4">make/4</a></td><td>Makes a new backend object.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_data-1"></a>

### get_data/1 ###

<pre><code>
get_data(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

Gets the backend data

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Gets the backend ID

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; module()
</code></pre>
<br />

Gets the backend module

<a name="get_process-1"></a>

### get_process/1 ###

<pre><code>
get_process(Logi_backend::<a href="#type-backend">backend()</a>) -&gt; <a href="#type-process">process()</a>
</code></pre>
<br />

Gets the backend process

<a name="is_backend-1"></a>

### is_backend/1 ###

<pre><code>
is_backend(Term::term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `Term` appears to be a backend, otherwise `false`

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Spec::<a href="#type-spec">spec()</a>) -&gt; <a href="#type-backend">backend()</a>
</code></pre>
<br />

Makes a new backend object from `Spec`

<a name="make-4"></a>

### make/4 ###

<pre><code>
make(Id::<a href="#type-id">id()</a>, Process::<a href="#type-process">process()</a>, Module::module(), Data::<a href="#type-data">data()</a>) -&gt; <a href="#type-backend">backend()</a>
</code></pre>
<br />

Makes a new backend object

