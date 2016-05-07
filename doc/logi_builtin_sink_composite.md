

# Module logi_builtin_sink_composite #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A built-in composite sink.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##


### <a name="NOTE">NOTE</a> ###

This module is provided for debuging/testing purposes only.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_children-1">get_children/1</a></td><td>Returns a list of children.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creats a new sink instance.</td></tr><tr><td valign="top"><a href="#set_active_writer-2">set_active_writer/2</a></td><td>Sets the <code>Nth</code> cihld to be active.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_children-1"></a>

### get_children/1 ###

<pre><code>
get_children(Pid::pid()) -&gt; [<a href="logi_sink.md#type-sink">logi_sink:sink()</a>]
</code></pre>
<br />

Returns a list of children

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Children::[<a href="logi_sink.md#type-sink">logi_sink:sink()</a>]) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creats a new sink instance

<a name="set_active_writer-2"></a>

### set_active_writer/2 ###

<pre><code>
set_active_writer(Pid::pid(), Nth::pos_integer()) -&gt; ok
</code></pre>
<br />

Sets the `Nth` cihld to be active

