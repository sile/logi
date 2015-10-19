

# Module logi_context #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Log Message Context.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-context">context()</a> ###


__abstract datatype__: `context()`

 A context



### <a name="type-map_form">map_form()</a> ###


<pre><code>
map_form() = #{channel =&gt; <a href="logi_channel.md#type-id">logi_channel:id()</a>, timestamp =&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>, severity =&gt; <a href="logi.md#type-severity">logi:severity()</a>, subject =&gt; term(), location =&gt; <a href="logi_location.md#type-location">logi_location:location()</a>, headers =&gt; <a href="logi.md#type-headers">logi:headers()</a>, metadata =&gt; <a href="logi.md#type-metadata">logi:metadata()</a>}
</code></pre>

 The map representation of a context

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Creates a new context object from <code>Map</code></td></tr><tr><td valign="top"><a href="#get_channel-1">get_channel/1</a></td><td>Gets the channel of <code>Context</code></td></tr><tr><td valign="top"><a href="#get_headers-1">get_headers/1</a></td><td>Gets the headers of <code>Context</code></td></tr><tr><td valign="top"><a href="#get_location-1">get_location/1</a></td><td>Gets the location of <code>Context</code></td></tr><tr><td valign="top"><a href="#get_metadata-1">get_metadata/1</a></td><td>Gets the metadata of <code>Context</code></td></tr><tr><td valign="top"><a href="#get_severity-1">get_severity/1</a></td><td>Gets the severity of <code>Context</code></td></tr><tr><td valign="top"><a href="#get_subject-1">get_subject/1</a></td><td>Gets the subject of <code>Context</code></td></tr><tr><td valign="top"><a href="#get_timestamp-1">get_timestamp/1</a></td><td>Gets the timestamp of <code>Context</code></td></tr><tr><td valign="top"><a href="#is_context-1">is_context/1</a></td><td>Returns <code>true</code> if <code>X</code> is a context object, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-7"><tt>new(Channel, os:timestamp(), Severity, undefined,
logi_location:guess_location(), #{}, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#new-7">new/7</a></td><td>Creates a new context object.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>Converts <code>Context</code> into a map form.</td></tr><tr><td valign="top"><a href="#unsafe_new-7">unsafe_new/7</a></td><td>Equivalent to <a href="#new-6"><code>new/6</code></a> except omission of the arguments validation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-map_form">map_form()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Creates a new context object from `Map`

<a name="get_channel-1"></a>

### get_channel/1 ###

<pre><code>
get_channel(Context::<a href="#type-context">context()</a>) -&gt; <a href="logi_channel.md#type-id">logi_channel:id()</a>
</code></pre>
<br />

Gets the channel of `Context`

<a name="get_headers-1"></a>

### get_headers/1 ###

<pre><code>
get_headers(Context::<a href="#type-context">context()</a>) -&gt; <a href="logi.md#type-headers">logi:headers()</a>
</code></pre>
<br />

Gets the headers of `Context`

<a name="get_location-1"></a>

### get_location/1 ###

<pre><code>
get_location(Context::<a href="#type-context">context()</a>) -&gt; <a href="logi_location.md#type-location">logi_location:location()</a>
</code></pre>
<br />

Gets the location of `Context`

<a name="get_metadata-1"></a>

### get_metadata/1 ###

<pre><code>
get_metadata(Context::<a href="#type-context">context()</a>) -&gt; <a href="logi.md#type-metadata">logi:metadata()</a>
</code></pre>
<br />

Gets the metadata of `Context`

<a name="get_severity-1"></a>

### get_severity/1 ###

<pre><code>
get_severity(Context::<a href="#type-context">context()</a>) -&gt; <a href="logi.md#type-severity">logi:severity()</a>
</code></pre>
<br />

Gets the severity of `Context`

<a name="get_subject-1"></a>

### get_subject/1 ###

<pre><code>
get_subject(Context::<a href="#type-context">context()</a>) -&gt; term()
</code></pre>
<br />

Gets the subject of `Context`

<a name="get_timestamp-1"></a>

### get_timestamp/1 ###

<pre><code>
get_timestamp(Context::<a href="#type-context">context()</a>) -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />

Gets the timestamp of `Context`

<a name="is_context-1"></a>

### is_context/1 ###

<pre><code>
is_context(X::<a href="#type-context">context()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a context object, `false` otherwise.

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Channel::<a href="logi_channel.md#type-id">logi_channel:id()</a>, Severity::<a href="logi.md#type-severity">logi:severity()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Equivalent to [`new(Channel, os:timestamp(), Severity, undefined,logi_location:guess_location(), #{}, #{})`](#new-7).

<a name="new-7"></a>

### new/7 ###

<pre><code>
new(Channel::<a href="logi_channel.md#type-id">logi_channel:id()</a>, Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>, Severity::<a href="logi.md#type-severity">logi:severity()</a>, Subject::term(), Location::<a href="logi_location.md#type-location">logi_location:location()</a>, Headers::<a href="logi.md#type-headers">logi:headers()</a>, Metadata::<a href="logi.md#type-metadata">logi:metadata()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Creates a new context object

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(C::<a href="#type-context">context()</a>) -&gt; <a href="#type-map_form">map_form()</a>
</code></pre>
<br />

Converts `Context` into a map form

<a name="unsafe_new-7"></a>

### unsafe_new/7 ###

<pre><code>
unsafe_new(Channel::<a href="logi_channel.md#type-id">logi_channel:id()</a>, Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>, Severity::<a href="logi.md#type-severity">logi:severity()</a>, Subject::term(), Location::<a href="logi_location.md#type-location">logi_location:location()</a>, Headers::<a href="logi.md#type-headers">logi:headers()</a>, Metadata::<a href="logi.md#type-metadata">logi:metadata()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Equivalent to [`new/6`](#new-6) except omission of the arguments validation

