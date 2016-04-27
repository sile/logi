

# Module logi_sink_writer #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `logi_sink_writer` behaviour.__<br /> Required callback functions: `write/4`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>

 A module that implements the `logi_sink` behaviour.



### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>




### <a name="type-writer">writer()</a> ###


__abstract datatype__: `writer()`




### <a name="type-written_data">written_data()</a> ###


<pre><code>
written_data() = <a href="logi_layout.md#type-formatted_data">logi_layout:formatted_data()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_callback_module-1">is_callback_module/1</a></td><td>Returns <code>true</code> if <code>X</code> is a module which implements the <code>writer</code> behaviour, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_writer-1">is_writer/1</a></td><td>Returns <code>true</code> if <code>X</code> is a writer instance, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new writer instance.</td></tr><tr><td valign="top"><a href="#write-4">write/4</a></td><td>Writes a log message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(X1::<a href="#type-writer">writer()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

<a name="get_state-1"></a>

### get_state/1 ###

<pre><code>
get_state(X1::<a href="#type-writer">writer()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

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
write(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Format::<a href="io.md#type-format">io:format()</a>, Data::<a href="logi_layout.md#type-data">logi_layout:data()</a>, X4::<a href="#type-writer">writer()</a>) -&gt; <a href="#type-written_data">written_data()</a>
</code></pre>
<br />

Writes a log message

If it fails to write, an exception will be raised.

