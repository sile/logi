

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#install-2">install/2</a></td><td>Equivalent to <a href="#install-3"><tt>install(Condition, Fun, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#install-3">install/3</a></td><td>Installs a sink which writes log messages by <code>Fun</code></td></tr><tr><td valign="top"><a href="#uninstall-0">uninstall/0</a></td><td>Equivalent to <a href="#uninstall-1"><tt>uninstall([])</tt></a>.</td></tr><tr><td valign="top"><a href="#uninstall-1">uninstall/1</a></td><td>Uninstalls a sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="install-2"></a>

### install/2 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Fun::<a href="#type-write_fun">write_fun()</a>) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>
<br />

Equivalent to [`install(Condition, Fun, [])`](#install-3).

<a name="install-3"></a>

### install/3 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Fun::<a href="#type-write_fun">write_fun()</a>, Options) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {id, <a href="logi_sink.md#type-id">logi_sink:id()</a>} | {channel, <a href="logi_channel.md#type-id">logi_channel:id()</a>} | <a href="logi_channel.md#type-install_sink_option">logi_channel:install_sink_option()</a></code></li></ul>

Installs a sink which writes log messages by `Fun`

The default value of `Options`: <br />
- id: `logi_builtin_sink_fun` <br />
- channel: `logi_channel:default_channel()` <br />

<a name="uninstall-0"></a>

### uninstall/0 ###

<pre><code>
uninstall() -&gt; <a href="logi_channel.md#type-uninstall_sink_result">logi_channel:uninstall_sink_result()</a>
</code></pre>
<br />

Equivalent to [`uninstall([])`](#uninstall-1).

<a name="uninstall-1"></a>

### uninstall/1 ###

<pre><code>
uninstall(Options) -&gt; <a href="logi_channel.md#type-uninstall_sink_result">logi_channel:uninstall_sink_result()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {id, <a href="logi_sink.md#type-id">logi_sink:id()</a>} | {channel, <a href="logi_channel.md#type-id">logi_channel:id()</a>}</code></li></ul>

Uninstalls a sink

The default value of `Options`: <br />
- id: `logi_builtin_sink_fun` <br />
- channel: `logi_channel:default_channel()` <br />

