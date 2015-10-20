

# Module logi_builtin_layout_simple #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A simple built-in layout.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="description"></a>

## Description ##
This module layouts a log message by the following format:

```
  {yyyy}-{MM}-{dd} {HH}:{mm}:{ss}.{SSS} [{SEVERITY}] {NODE} {PID} {MODULE}:{FUNCTION}:{LINE} [{HEADER(KEY=VALUE)}*] {MESSAGE}
```


### <a name="NOTE">NOTE</a> ###

This module is provided for debuging/testing purposes only.
The message format is not customizable.
And no overload protection exists (e.g. if log message is too large, the caller process may hang).


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > Layout = logi_builtin_layout_simple:new().
  > logi_builtin_sink_io_device:install(info, [{layout, Layout}]).
  > logi:info("hello world").
  2015-10-21 15:06:42.842 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Creates a layout.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Creates a layout

