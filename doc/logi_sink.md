

# Module logi_sink #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Sinks.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##

A sink has the specification of a sink process ([`logi_sink_proc`](logi_sink_proc.md)).
A sink process manages a sink writer ([`logi_sink_writer`](logi_sink_writer.md)).

See `logi_builtin_sink_XXX` modules for usage examples.

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = term()
</code></pre>

 The identifier of a sink

The scope of an identifier is limited in siblings with the same parent.



### <a name="type-sink">sink()</a> ###


__abstract datatype__: `sink()`

 A sink



### <a name="type-spec">spec()</a> ###


<pre><code>
spec() = <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>

 The specification of a sink process.

See official documents of `supervisor` for more information.

NOTE: `restart` field is ignored (always regarded as `permanent`).



### <a name="type-sup_flags">sup_flags()</a> ###


<pre><code>
sup_flags() = <a href="supervisor.md#type-sup_flags">supervisor:sup_flags()</a>
</code></pre>

 The supervise flags of a sink process.

See official documents of `supervisor` for more information.

NOTE: `strategy` field is ignored.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_writer-2">from_writer/2</a></td><td>Creates a sink from standalone a writer instance.</td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td>Equivalent to <a href="maps.md#get-2"><tt>maps:get(id, get_spec(Sink))</tt></a>.</td></tr><tr><td valign="top"><a href="#get_spec-1">get_spec/1</a></td><td>Gets the process specification of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_sup_flags-1">get_sup_flags/1</a></td><td>Gets the supervise flags of <code>Sink</code></td></tr><tr><td valign="top"><a href="#is_sink-1">is_sink/1</a></td><td>Returns <code>true</code> if <code>X</code> is a sink, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Spec, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_writer-2"></a>

### from_writer/2 ###

<pre><code>
from_writer(Id::<a href="#type-id">id()</a>, Writer::<a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Creates a sink from standalone a writer instance

No specific sink process is needed by `Writer` to write log messages.

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Equivalent to [`maps:get(id, get_spec(Sink))`](maps.md#get-2).

<a name="get_spec-1"></a>

### get_spec/1 ###

<pre><code>
get_spec(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-spec">spec()</a>
</code></pre>
<br />

Gets the process specification of `Sink`

The type of the return value is always map.

<a name="get_sup_flags-1"></a>

### get_sup_flags/1 ###

<pre><code>
get_sup_flags(Logi_sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-sup_flags">sup_flags()</a>
</code></pre>
<br />

Gets the supervise flags of `Sink`

The type of the return value is always map.

<a name="is_sink-1"></a>

### is_sink/1 ###

<pre><code>
is_sink(X::<a href="#type-sink">sink()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a sink, `false` otherwise

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Spec::<a href="#type-spec">spec()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Equivalent to [`new(Spec, #{})`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Spec::<a href="#type-spec">spec()</a>, Flags::<a href="#type-sup_flags">sup_flags()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Creates a new sink

