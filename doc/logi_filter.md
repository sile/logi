

# Module logi_filter #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Log Message Filter Behaviour.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_filter` behaviour.__<br /> Required callback functions: `filter/2`.

<a name="description"></a>

## Description ##

A filter decides whether to allow a message be sent to the target channel.


### <a name="NOTE">NOTE</a> ###

A filter should not raise exceptions when it's `filter/2` is called.

If any exception is raised, the invocation of the log function will be aborted and
the exception will be propagated to the caller process.


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
  > Context0 = logi_context:new(sample_log, info).
  > FilterFun = fun (C) -> not maps:get(discard, logi_context:get_metadata(C), false) end.
  > Filter = logi_builtin_filter_fun:new(FilterFun).
  > logi_filter:apply(Context0, Filter).
  true
  > Context1 = logi_context:from_map(maps:put(metadata, #{discard => true}, logi_context:to_map(Context0))).
  > logi_filter:apply(Context1, Filter).
  false
```

A more realistic example:

```erlang

  > WriteFun = fun (_, _, Format, Data) -> io:format(Format ++ "\n", Data) end.
  > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun)).
  > FilterFun = fun (C) -> not maps:get(discard, logi_context:get_metadata(C), false) end.
  > Logger = logi:new([{filter, logi_builtin_filter_fun:new(FilterFun)}]).
  > logi:save_as_default(Logger).
  > logi:info("hello world").
  hello world
  > logi:info("hello world", [], [{metadata, #{discard => true}}]).
  % No output: the log message was discarded by the filter
```

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>

 A module that implements the `logi_filter` behaviour.



### <a name="type-filter">filter()</a> ###


__abstract datatype__: `filter()`

 An instance of `logi_filter` behaviour implementation module.



### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>

 The value of the second arguemnt of the `filter/2` callback function.

If the `filter()` does not have an explicit `state()`, `undefined` will be passed instead.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply-2">apply/2</a></td><td>Applies <code>Filter</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Filter</code></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td>Gets the state of <code>Filter</code></td></tr><tr><td valign="top"><a href="#is_filter-1">is_filter/1</a></td><td>Returns <code>true</code> if <code>X</code> is a filter, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Module, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new filter instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply-2"></a>

### apply/2 ###

<pre><code>
apply(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Filter::<a href="#type-filter">filter()</a>) -&gt; DoAllow | {DoAllow, NewFilter}
</code></pre>

<ul class="definitions"><li><code>DoAllow = boolean()</code></li><li><code>NewFilter = <a href="#type-filter">filter()</a></code></li></ul>

Applies `Filter`

This function returns `DoAllow` if the state of `Filter` is not changed, `{DoAllow, NewFilter}` otherwise.

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Filter::<a href="#type-filter">filter()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

Gets the module of `Filter`

<a name="get_state-1"></a>

### get_state/1 ###

<pre><code>
get_state(Filter::<a href="#type-filter">filter()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Gets the state of `Filter`

<a name="is_filter-1"></a>

### is_filter/1 ###

<pre><code>
is_filter(X::<a href="#type-filter">filter()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a filter, `false` otherwise

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-filter">filter()</a>
</code></pre>
<br />

Equivalent to [`new(Module, undefined)`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, State::<a href="#type-state">state()</a>) -&gt; <a href="#type-filter">filter()</a>
</code></pre>
<br />

Creates a new filter instance

