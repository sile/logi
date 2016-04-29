

# Module logi_sink #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = term()
</code></pre>




### <a name="type-sink">sink()</a> ###


<pre><code>
sink() = #logi_sink{spec = undefined | <a href="#type-spec">spec()</a>, sup_flags = undefined | <a href="#type-sup_flags">sup_flags()</a>}
</code></pre>




### <a name="type-spec">spec()</a> ###


<pre><code>
spec() = <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>




### <a name="type-sup_flags">sup_flags()</a> ###


<pre><code>
sup_flags() = <a href="supervisor.md#type-sup_flags">supervisor:sup_flags()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_writer-2">from_writer/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_spec-1">get_spec/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_sup_flags-1">get_sup_flags/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_sink-1">is_sink/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_writer-2"></a>

### from_writer/2 ###

<pre><code>
from_writer(Id::<a href="#type-id">id()</a>, Writer::<a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Logi_sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

<a name="get_spec-1"></a>

### get_spec/1 ###

<pre><code>
get_spec(Logi_sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-spec">spec()</a>
</code></pre>
<br />

<a name="get_sup_flags-1"></a>

### get_sup_flags/1 ###

<pre><code>
get_sup_flags(Logi_sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-sup_flags">sup_flags()</a>
</code></pre>
<br />

<a name="is_sink-1"></a>

### is_sink/1 ###

<pre><code>
is_sink(X::<a href="#type-sink">sink()</a> | term()) -&gt; boolean()
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Spec::<a href="#type-spec">spec()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Spec::<a href="#type-spec">spec()</a>, Flags::<a href="#type-sup_flags">sup_flags()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

