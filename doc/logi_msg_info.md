

# Module logi_msg_info #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The additional informations on a log message.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##
TODO(?): Renames to logi_message
<a name="types"></a>

## Data Types ##




### <a name="type-info">info()</a> ###


__abstract datatype__: `info()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_headers-1">get_headers/1</a></td><td>Gets the headers of the message.</td></tr><tr><td valign="top"><a href="#get_location-1">get_location/1</a></td><td>Gets the location of the message.</td></tr><tr><td valign="top"><a href="#get_metadata-1">get_metadata/1</a></td><td>Gets the metadata of the message.</td></tr><tr><td valign="top"><a href="#get_severity-1">get_severity/1</a></td><td>Gets the severity of the message.</td></tr><tr><td valign="top"><a href="#get_timestamp-1">get_timestamp/1</a></td><td>Gets the timestamp of the message.</td></tr><tr><td valign="top"><a href="#make-5">make/5</a></td><td>Makes a <code>info()</code> object.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_headers-1"></a>

### get_headers/1 ###

<pre><code>
get_headers(Logi_msg_info::<a href="#type-info">info()</a>) -&gt; <a href="logi.md#type-headers">logi:headers()</a>
</code></pre>
<br />

Gets the headers of the message

<a name="get_location-1"></a>

### get_location/1 ###

<pre><code>
get_location(Logi_msg_info::<a href="#type-info">info()</a>) -&gt; <a href="logi_location.md#type-location">logi_location:location()</a>
</code></pre>
<br />

Gets the location of the message

<a name="get_metadata-1"></a>

### get_metadata/1 ###

<pre><code>
get_metadata(Logi_msg_info::<a href="#type-info">info()</a>) -&gt; <a href="logi.md#type-metadata">logi:metadata()</a>
</code></pre>
<br />

Gets the metadata of the message

<a name="get_severity-1"></a>

### get_severity/1 ###

<pre><code>
get_severity(Logi_msg_info::<a href="#type-info">info()</a>) -&gt; <a href="logi.md#type-severity">logi:severity()</a>
</code></pre>
<br />

Gets the severity of the message

<a name="get_timestamp-1"></a>

### get_timestamp/1 ###

<pre><code>
get_timestamp(Logi_msg_info::<a href="#type-info">info()</a>) -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />

Gets the timestamp of the message

<a name="make-5"></a>

### make/5 ###

<pre><code>
make(Severity::<a href="logi.md#type-severity">logi:severity()</a>, Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>, Location::<a href="logi_location.md#type-location">logi_location:location()</a>, Headers::<a href="logi.md#type-headers">logi:headers()</a>, MetaData::<a href="logi.md#type-metadata">logi:metadata()</a>) -&gt; <a href="#type-info">info()</a>
</code></pre>
<br />

Makes a `info()` object

