

# Module logi_builtin_layout_pass_through #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A built-in no-op layout.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="description"></a>

## Description ##

This layout simply returns original `Format` and `Data` as tuple format (i.e. `{Format, Data}`).


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
  > Context = logi_context:new(sample_log, info).
  > Layout = logi_builtin_layout_pass_through:new().
  > logi_layout:format(Context, "hello ~s", [world], Layout).
  {"hello ~s", [world]}
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Creates a new layout instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout</a>({<a href="io.md#type-format">io:format()</a>, <a href="logi_layout.md#type-data">logi_layout:data()</a>})
</code></pre>
<br />

Creates a new layout instance

