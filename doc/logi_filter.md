

# Module logi_filter #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_filter` behaviour.__<br /> Required callback functions: `filter/3`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>




### <a name="type-filter">filter()</a> ###


__abstract datatype__: `filter()`




### <a name="type-option">option()</a> ###


<pre><code>
option() = {Key::term(), Value::term()}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply-3">apply/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_filter-1">is_filter/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply-3"></a>

### apply/3 ###

<pre><code>
apply(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Options::<a href="#type-options">options()</a>, X3::<a href="#type-filter">filter()</a>) -&gt; {boolean(), <a href="#type-filter">filter()</a>}
</code></pre>
<br />

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map) -&gt; <a href="#type-filter">filter()</a>
</code></pre>

<ul class="definitions"><li><code>Map = #{module =&gt; <a href="#type-callback_module">callback_module()</a>, state =&gt; <a href="#type-state">state()</a>}</code></li></ul>

<a name="is_filter-1"></a>

### is_filter/1 ###

<pre><code>
is_filter(X1::term()) -&gt; boolean()
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-filter">filter()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, State::<a href="#type-state">state()</a>) -&gt; <a href="#type-filter">filter()</a>
</code></pre>
<br />

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(X::<a href="#type-filter">filter()</a>) -&gt; #{module =&gt; <a href="#type-callback_module">callback_module()</a>, state =&gt; <a href="#type-state">state()</a>}
</code></pre>
<br />

