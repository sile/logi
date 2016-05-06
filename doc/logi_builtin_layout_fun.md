

# Module logi_builtin_layout_fun #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A built-in layout which formats log messages by an arbitrary user defined function.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="description"></a>

## Description ##

This layout formats log messages by `format_fun/0` which was specified by the argument of [`new/1`](#new-1).


### <a name="NOTE">NOTE</a> ###

This module is provided for debuging/testing purposes only.

A layout will be stored into a logi_channel's ETS.
Then it will be loaded every time a log message is issued.
Therefore if the format function (`format_fun/0`) of the layout is a huge size anonymous function,
all log issuers which use the channel will have to pay a non negligible cost to load it.


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
  > Context = logi_context:new(sample_log, info).
  > FormatFun = fun (_, Format, Data) -> io_lib:format("EXAMPLE: " ++ Format, Data) end.
  > Layout = logi_builtin_layout_fun:new(FormatFun).
  > lists:flatten(logi_layout:format(Context, "Hello ~s", ["World"], Layout)).
  "EXAMPLE: Hello World"
```

A layout used by a sink can be specified at the time of installing the sink:

```erlang

  > Layout0 = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[LAYOUT_0] " ++ Format ++ "\n", Data) end).
  > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(foo, [{layout, Layout0}]), info).
  > logi:info("hello world").
  [LAYOUT_0] hello world
  > Layout1 = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[LAYOUT_1] " ++ Format ++ "\n", Data) end).
  > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(bar, [{layout, Layout1}]), info).
  > logi:info("hello world").
  [LAYOUT_0] hello world
  [LAYOUT_1] hello world
```

<a name="types"></a>

## Data Types ##




### <a name="type-format_fun">format_fun()</a> ###


<pre><code>
format_fun() = fun((<a href="logi_context.md#type-context">logi_context:context()</a>, <a href="io.md#type-format">io:format()</a>, <a href="logi_layout.md#type-data">logi_layout:data()</a>) -&gt; <a href="logi_layout.md#type-formatted_data">logi_layout:formatted_data()</a>)
</code></pre>

 A log message formatting function

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a layout which formats log messages by <code>FormatFun</code></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(FormatFun::<a href="#type-format_fun">format_fun()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Creates a layout which formats log messages by `FormatFun`

