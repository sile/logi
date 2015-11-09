

# Module logi_builtin_sink_fun #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A built-in sink which consumes log messages by an arbitrary user defined function.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="description"></a>

## Description ##

The default layout is `logi_builtin_layout_simple:new()`.


### <a name="NOTE">NOTE</a> ###

This module is provided for debuging/testing purposes only.

A sink is stored into a logi_channel's ETS.
Then it will be loaded every time a log message is issued.
Therefore if the write function (`write_fun/0`) of the sink is a huge size anonymous function,
all log issuers which use the channel will have to pay a non negligible cost to load it.

And there is no overload protection.


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > WriteFun = fun (_, _, Format, Data) -> io:format("[CONSUMED] " ++ Format ++ "\n", Data) end.
  > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun).
  > logi:info("hello world").
  [CONSUMED] hello world
```

<a name="types"></a>

## Data Types ##




### <a name="type-write_fun">write_fun()</a> ###


<pre><code>
write_fun() = fun((<a href="logi_context.md#type-context">logi_context:context()</a>, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>, <a href="io.md#type-format">io:format()</a>, <a href="logi_layout.md#type-data">logi_layout:data()</a>) -&gt; any())
</code></pre>

 A function which is used to consume log messages issued by `logi`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creats a new sink instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Fun::<a href="#type-write_fun">write_fun()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creats a new sink instance
