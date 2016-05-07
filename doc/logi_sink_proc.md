

# Module logi_sink_proc #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Functions for sink processes.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##

A sink process manages the lifetime of a sink and a sink writer instance([`logi_sink_writer`](logi_sink_writer.md)).

Sink process is spawned at time a sink is installed in a channel ([`logi_channel:install_sink/2`](logi_channel.md#install_sink-2)).

After spawned, the process should call [`send_writer_to_parent/1`](#send_writer_to_parent-1) to
notify available writer instance to the parent.

If the root sink process exits, the associated sink is uninstalled from the channel.

<a name="types"></a>

## Data Types ##




### <a name="type-sink_sup">sink_sup()</a> ###


<pre><code>
sink_sup() = pid()
</code></pre>

 The supervisor of a sink process

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#recv_writer_from_child-2">recv_writer_from_child/2</a></td><td>Receives a sink writer instance from the child sink process <code>SinkSup</code></td></tr><tr><td valign="top"><a href="#send_writer_to_parent-1">send_writer_to_parent/1</a></td><td>Sends <code>Writer</code> to the parent sink process.</td></tr><tr><td valign="top"><a href="#start_child-1">start_child/1</a></td><td>Starts a new child sink process.</td></tr><tr><td valign="top"><a href="#stop_child-1">stop_child/1</a></td><td>Stops the sink process.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="recv_writer_from_child-2"></a>

### recv_writer_from_child/2 ###

<pre><code>
recv_writer_from_child(SinkSup::<a href="#type-sink_sup">sink_sup()</a>, Timeout::timeout()) -&gt; <a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a> | undefined
</code></pre>
<br />

Receives a sink writer instance from the child sink process `SinkSup`

<a name="send_writer_to_parent-1"></a>

### send_writer_to_parent/1 ###

<pre><code>
send_writer_to_parent(Writer::<a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a> | undefined) -&gt; ok
</code></pre>
<br />

Sends `Writer` to the parent sink process

NOTICE: This function can only be invoked in a sink process.

<a name="start_child-1"></a>

### start_child/1 ###

<pre><code>
start_child(Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; {ok, <a href="#type-sink_sup">sink_sup()</a>} | {error, Reason::term()}
</code></pre>
<br />

Starts a new child sink process

NOTICE: This function can only be invoked in a sink process.

<a name="stop_child-1"></a>

### stop_child/1 ###

<pre><code>
stop_child(SinkSup::<a href="#type-sink_sup">sink_sup()</a>) -&gt; ok
</code></pre>
<br />

Stops the sink process

NOTICE: This function can only be invoked in a sink process.

