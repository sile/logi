

# Module logi_layout #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Log Message Layout Behaviour.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_layout` behaviour.__<br /> Required callback functions: `format/4`.

<a name="description"></a>

## Description ##

This module defines the standard interface to format log messages issued by `logi` functions
(e.g. [`logi:info/3`](logi.md#info-3), [`logi:warning/3`](logi.md#warning-3), etc).


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > Context = logi_context:new(sample_log, info).
  > FormatFun = fun (_, Format, Data) -> lists:flatten(io_lib:format("EXAMPLE: " ++ Format, Data)) end.
  > Layout = logi_builtin_layout_fun:new(FormatFun).
  > logi_layout:format(Context, "Hello ~s", ["World"], Layout).
  "EXAMPLE: Hello World"
```

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>

 A module that implements the `logi_layout` behaviour.



### <a name="type-data">data()</a> ###


<pre><code>
data() = [term()]
</code></pre>

 A data which is subject to format

This type is an alias of the type of second arguemnt of the [`io_lib:format/2`](io_lib.md#format-2)



### <a name="type-extra_data">extra_data()</a> ###


<pre><code>
extra_data() = term()
</code></pre>

 The value of the fourth arguemnt of the `format/4` callback function.

If the `layout()` does not have an explicit `extra_data()`, `undefined` will be passed instead.



### <a name="type-layout">layout()</a> ###


__abstract datatype__: `layout(ExtraData)`

 A specialized type of `layout/0`.
This may be useful for modules which want to annotate their own `ExtraData` type.



### <a name="type-layout">layout()</a> ###


<pre><code>
layout() = <a href="#type-layout">layout</a>(<a href="#type-extra_data">extra_data()</a>)
</code></pre>

 An instance of `logi_layout` behaviour implementation module.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-4">format/4</a></td><td>Returns an <code>iodata()</code> which represents <code>Data</code> formatted by <code>Layout</code> in accordance with <code>Format</code> and <code>Context</code></td></tr><tr><td valign="top"><a href="#get_extra_data-1">get_extra_data/1</a></td><td>Gets the extra data of <code>Layout</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Layout</code></td></tr><tr><td valign="top"><a href="#is_layout-1">is_layout/1</a></td><td>Returns <code>true</code> if <code>X</code> is a layout, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Module, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new layout instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-4"></a>

### format/4 ###

<pre><code>
format(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Format::<a href="io.md#type-format">io:format()</a>, Data::<a href="#type-data">data()</a>, Layout::<a href="#type-layout">layout()</a>) -&gt; iodata()
</code></pre>
<br />

Returns an `iodata()` which represents `Data` formatted by `Layout` in accordance with `Format` and `Context`

<a name="get_extra_data-1"></a>

### get_extra_data/1 ###

<pre><code>
get_extra_data(Layout::<a href="#type-layout">layout()</a>) -&gt; <a href="#type-extra_data">extra_data()</a>
</code></pre>
<br />

Gets the extra data of `Layout`

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Layout::<a href="#type-layout">layout()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

Gets the module of `Layout`

<a name="is_layout-1"></a>

### is_layout/1 ###

<pre><code>
is_layout(X::<a href="#type-layout">layout()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a layout, `false` otherwise

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-layout">layout()</a>
</code></pre>
<br />

Equivalent to [`new(Module, undefined)`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, ExtraData) -&gt; <a href="#type-layout">layout</a>(ExtraData)
</code></pre>

<ul class="definitions"><li><code>ExtraData = <a href="#type-extra_data">extra_data()</a></code></li></ul>

Creates a new layout instance

