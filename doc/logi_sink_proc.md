

# Module logi_sink_proc #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-child_id">child_id()</a> ###


<pre><code>
child_id() = pid()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#recv_writer_from_child-2">recv_writer_from_child/2</a></td><td></td></tr><tr><td valign="top"><a href="#send_writer_to_parent-1">send_writer_to_parent/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_child-1">start_child/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop_child-1">stop_child/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="recv_writer_from_child-2"></a>

### recv_writer_from_child/2 ###

<pre><code>
recv_writer_from_child(ChildId::<a href="#type-child_id">child_id()</a>, Timeout::timeout()) -&gt; <a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a> | undefined
</code></pre>
<br />

<a name="send_writer_to_parent-1"></a>

### send_writer_to_parent/1 ###

<pre><code>
send_writer_to_parent(Writer::<a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a> | undefined) -&gt; ok
</code></pre>
<br />

<a name="start_child-1"></a>

### start_child/1 ###

<pre><code>
start_child(Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; {ok, <a href="#type-child_id">child_id()</a>} | {error, Reason::term()}
</code></pre>
<br />

<a name="stop_child-1"></a>

### stop_child/1 ###

<pre><code>
stop_child(ChildId::<a href="#type-child_id">child_id()</a>) -&gt; ok
</code></pre>
<br />

