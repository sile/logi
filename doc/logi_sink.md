

# Module logi_sink #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Sink object.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_sink` behaviour.__<br /> Required callback functions: `append/4`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>




### <a name="type-condition">condition()</a> ###


<pre><code>
condition() = <a href="#type-severity_condition">severity_condition()</a> | <a href="#type-location_condition">location_condition()</a>
</code></pre>




### <a name="type-expanded_condition">expanded_condition()</a> ###


<pre><code>
expanded_condition() = [<a href="logi.md#type-log_level">logi:log_level()</a> | {<a href="logi.md#type-log_level">logi:log_level()</a>, atom()} | {<a href="logi.md#type-log_level">logi:log_level()</a>, atom(), module()}]
</code></pre>




### <a name="type-extra_data">extra_data()</a> ###


<pre><code>
extra_data() = term()
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>




### <a name="type-location_condition">location_condition()</a> ###


<pre><code>
location_condition() = #{severity =&gt; <a href="#type-severity_condition">severity_condition()</a>, application =&gt; atom() | [atom()], module =&gt; module() | [module()]}
</code></pre>




### <a name="type-severity_condition">severity_condition()</a> ###


<pre><code>
severity_condition() = <a href="logi.md#type-log_level">logi:log_level()</a> | {<a href="logi.md#type-log_level">logi:log_level()</a>, <a href="logi.md#type-log_level">logi:log_level()</a>} | [<a href="logi.md#type-log_level">logi:log_level()</a>]
</code></pre>




### <a name="type-sink">sink()</a> ###


__abstract datatype__: `sink()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Makes a new sink from a map.</td></tr><tr><td valign="top"><a href="#get_condition-1">get_condition/1</a></td><td>Gets the condition of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_expanded_condition-1">get_expanded_condition/1</a></td><td>Gets the expanded condition of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_extra_data-1">get_extra_data/1</a></td><td>Gets the extra data of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td>Gets the ID of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Sink</code></td></tr><tr><td valign="top"><a href="#is_sink-1">is_sink/1</a></td><td>TODO.</td></tr><tr><td valign="top"><a href="#is_valid_condition-1">is_valid_condition/1</a></td><td>Returns <code>true</code> if <code>X</code> is a valid <code>condition()</code> value, and <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(Id, Module, debug)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Equivalent to <a href="#new-4"><tt>new(Id, Module, Condition, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Makes a new sink.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>Converts <code>Sink</code> into a map.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map) -&gt; <a href="#type-sink">sink()</a>
</code></pre>

<ul class="definitions"><li><code>Map = #{id =&gt; <a href="#type-id">id()</a>, module =&gt; <a href="#type-callback_module">callback_module()</a>, condition =&gt; <a href="#type-condition">condition()</a>, extra_data =&gt; <a href="#type-extra_data">extra_data()</a>}</code></li></ul>

Makes a new sink from a map

Default Value: <br />
- id: none (mandatory) <br />
- module: none (mandatory) <br />
- condition: `debug` <br />
- extra_data: `undefined` <br />

<a name="get_condition-1"></a>

### get_condition/1 ###

<pre><code>
get_condition(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-condition">condition()</a>
</code></pre>
<br />

Gets the condition of `Sink`

<a name="get_expanded_condition-1"></a>

### get_expanded_condition/1 ###

<pre><code>
get_expanded_condition(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-expanded_condition">expanded_condition()</a>
</code></pre>
<br />

Gets the expanded condition of `Sink`

<a name="get_extra_data-1"></a>

### get_extra_data/1 ###

<pre><code>
get_extra_data(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-extra_data">extra_data()</a>
</code></pre>
<br />

Gets the extra data of `Sink`

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Gets the ID of `Sink`

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

Gets the module of `Sink`

<a name="is_sink-1"></a>

### is_sink/1 ###

<pre><code>
is_sink(X::<a href="#type-sink">sink()</a> | term()) -&gt; boolean()
</code></pre>
<br />

TODO

<a name="is_valid_condition-1"></a>

### is_valid_condition/1 ###

<pre><code>
is_valid_condition(X::<a href="#type-condition">condition()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a valid `condition()` value, and `false` otherwise

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="#type-id">id()</a>, Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Equivalent to [`new(Id, Module, debug)`](#new-3).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Id::<a href="#type-id">id()</a>, Module::<a href="#type-callback_module">callback_module()</a>, Condition::<a href="#type-condition">condition()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Equivalent to [`new(Id, Module, Condition, undefined)`](#new-4).

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(Id::<a href="#type-id">id()</a>, Module::<a href="#type-callback_module">callback_module()</a>, Condition::<a href="#type-condition">condition()</a>, ExtraData::<a href="#type-extra_data">extra_data()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Makes a new sink

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Sink::<a href="#type-sink">sink()</a>) -&gt; Map
</code></pre>

<ul class="definitions"><li><code>Map = #{id =&gt; <a href="#type-id">id()</a>, module =&gt; <a href="#type-callback_module">callback_module()</a>, condition =&gt; <a href="#type-condition">condition()</a>, extra_data =&gt; <a href="#type-extra_data">extra_data()</a>}</code></li></ul>

Converts `Sink` into a map

