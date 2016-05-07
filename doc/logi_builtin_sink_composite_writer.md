

# Module logi_builtin_sink_composite_writer #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A built-in composite sink process and writer.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`gen_server`](gen_server.md), [`logi_sink_writer`](logi_sink_writer.md).

<a name="description"></a>

## Description ##


### <a name="NOTE">NOTE</a> ###

This module is provided for debuging/testing purposes only.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_active_writer-1">get_active_writer/1</a></td><td>Gets the active child writer.</td></tr><tr><td valign="top"><a href="#get_children-1">get_children/1</a></td><td>Returns a list of children.</td></tr><tr><td valign="top"><a href="#set_active_writer-2">set_active_writer/2</a></td><td>Sets the <code>Nth</code> cihld to be active.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts a new process.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_active_writer-1"></a>

### get_active_writer/1 ###

<pre><code>
get_active_writer(Pid::pid()) -&gt; <a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a> | undefined
</code></pre>
<br />

Gets the active child writer

<a name="get_children-1"></a>

### get_children/1 ###

<pre><code>
get_children(Pid::pid()) -&gt; [<a href="logi_sink.md#type-sink">logi_sink:sink()</a>]
</code></pre>
<br />

Returns a list of children

<a name="set_active_writer-2"></a>

### set_active_writer/2 ###

<pre><code>
set_active_writer(Pid::pid(), Nth::pos_integer()) -&gt; ok
</code></pre>
<br />

Sets the `Nth` cihld to be active

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Children::[<a href="logi_sink.md#type-sink">logi_sink:sink()</a>]) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Starts a new process

