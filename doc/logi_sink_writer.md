

# Module logi_sink_writer #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Sink Writer Behaviour.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_sink_writer` behaviour.__<br /> Required callback functions: `write/4`, `get_writee/1`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>

 A module that implements the `logi_sink_writer` behaviour.



### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>

 The value of the fourth arguemnt of the `write/4` callback function.

NOTE:
This value might be loaded from ETS every time when a log message is issued.
Therefore, very huge state can cause a performance problem.



### <a name="type-writer">writer()</a> ###


__abstract datatype__: `writer()`

 A writer instance.



### <a name="type-written_data">written_data()</a> ###


<pre><code>
written_data() = <a href="logi_layout.md#type-formatted_data">logi_layout:formatted_data()</a>
</code></pre>

 The data written to a sink

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Writer</code></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td>Gets the state of <code>Writer</code></td></tr><tr><td valign="top"><a href="#get_writee-1">get_writee/1</a></td><td>Gets the writee process of log messages.</td></tr><tr><td valign="top"><a href="#is_callback_module-1">is_callback_module/1</a></td><td>Returns <code>true</code> if <code>X</code> is a module which implements the <code>writer</code> behaviour, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_writer-1">is_writer/1</a></td><td>Returns <code>true</code> if <code>X</code> is a writer instance, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new writer instance.</td></tr><tr><td valign="top"><a href="#write-4">write/4</a></td><td>Writes a log message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Writer::<a href="#type-writer">writer()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

Gets the module of `Writer`

<a name="get_state-1"></a>

### get_state/1 ###

<pre><code>
get_state(Writer::<a href="#type-writer">writer()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Gets the state of `Writer`

<a name="get_writee-1"></a>

### get_writee/1 ###

<pre><code>
get_writee(Writer::<a href="#type-writer">writer()</a>) -&gt; pid() | undefined
</code></pre>
<br />

Gets the writee process of log messages

"writee" is the destination process of `written_data()` of [`write/4`](#write-4).

If such process is dead or unknown, the function returns `undefined`.

The result value might change on every call.

<a name="is_callback_module-1"></a>

### is_callback_module/1 ###

<pre><code>
is_callback_module(X::<a href="#type-callback_module">callback_module()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a module which implements the `writer` behaviour, otherwise `false`

<a name="is_writer-1"></a>

### is_writer/1 ###

<pre><code>
is_writer(X::<a href="#type-writer">writer()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a writer instance, otherwise `false`

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, State::<a href="#type-state">state()</a>) -&gt; <a href="#type-writer">writer()</a>
</code></pre>
<br />

Creates a new writer instance

<a name="write-4"></a>

### write/4 ###

<pre><code>
write(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Format::<a href="io.md#type-format">io:format()</a>, Data::<a href="logi_layout.md#type-data">logi_layout:data()</a>, Writer::<a href="#type-writer">writer()</a>) -&gt; <a href="#type-written_data">written_data()</a>
</code></pre>
<br />

Writes a log message

If it fails to write, an exception will be raised.

