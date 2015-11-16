

# Module logi_restart_strategy #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_restart_strategy` behaviour.__<br /> Required callback functions: `next/1`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>




### <a name="type-strategy">strategy()</a> ###


__abstract datatype__: `strategy()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_strategy-1">is_strategy/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#next-1">next/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(X1::<a href="#type-strategy">strategy()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

<a name="get_state-1"></a>

### get_state/1 ###

<pre><code>
get_state(X1::<a href="#type-strategy">strategy()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

<a name="is_strategy-1"></a>

### is_strategy/1 ###

<pre><code>
is_strategy(Module::<a href="#type-strategy">strategy()</a> | term()) -&gt; boolean()
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-strategy">strategy()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, State::<a href="#type-state">state()</a>) -&gt; <a href="#type-strategy">strategy()</a>
</code></pre>
<br />

<a name="next-1"></a>

### next/1 ###

<pre><code>
next(X1::<a href="#type-strategy">strategy()</a>) -&gt; {ok, timeout(), <a href="#type-strategy">strategy()</a>} | stop
</code></pre>
<br />

