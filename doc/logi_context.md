

# Module logi_context #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-context">context()</a> ###


__abstract datatype__: `context()`




### <a name="type-context_map">context_map()</a> ###


<pre><code>
context_map() = #{channel =&gt; <a href="logi_channel.md#type-id">logi_channel:id()</a>, timestamp =&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>, severity =&gt; <a href="logi.md#type-severity">logi:severity()</a>, location =&gt; <a href="logi_location.md#type-location">logi_location:location()</a>, headers =&gt; <a href="logi.md#type-headers">logi:headers()</a>, metadata =&gt; <a href="logi.md#type-metadata">logi:metadata()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_channel-1">get_channel/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_headers-1">get_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_location-1">get_location/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_metadata-1">get_metadata/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_severity-1">get_severity/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_timestamp-1">get_timestamp/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_context-1">is_context/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_channel-1"></a>

### get_channel/1 ###

<pre><code>
get_channel(Logi_context::<a href="#type-context">context()</a>) -&gt; <a href="logi_channel.md#type-id">logi_channel:id()</a>
</code></pre>
<br />

<a name="get_headers-1"></a>

### get_headers/1 ###

<pre><code>
get_headers(Logi_context::<a href="#type-context">context()</a>) -&gt; <a href="logi.md#type-headers">logi:headers()</a>
</code></pre>
<br />

<a name="get_location-1"></a>

### get_location/1 ###

<pre><code>
get_location(Logi_context::<a href="#type-context">context()</a>) -&gt; <a href="logi_location.md#type-location">logi_location:location()</a>
</code></pre>
<br />

<a name="get_metadata-1"></a>

### get_metadata/1 ###

<pre><code>
get_metadata(Logi_context::<a href="#type-context">context()</a>) -&gt; <a href="logi.md#type-metadata">logi:metadata()</a>
</code></pre>
<br />

<a name="get_severity-1"></a>

### get_severity/1 ###

<pre><code>
get_severity(Logi_context::<a href="#type-context">context()</a>) -&gt; <a href="logi.md#type-severity">logi:severity()</a>
</code></pre>
<br />

<a name="get_timestamp-1"></a>

### get_timestamp/1 ###

<pre><code>
get_timestamp(Logi_context::<a href="#type-context">context()</a>) -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />

<a name="is_context-1"></a>

### is_context/1 ###

<pre><code>
is_context(X::<a href="#type-context">context()</a> | term()) -&gt; boolean()
</code></pre>
<br />

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(C::<a href="#type-context">context()</a>) -&gt; <a href="#type-context_map">context_map()</a>
</code></pre>
<br />

