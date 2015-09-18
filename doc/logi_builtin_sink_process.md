

# Module logi_builtin_sink_process #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A built-in process sink.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##

NOTE: This module is provided for debuging/testing purposes only.
(e.g. Overload protection is missing)

```
  %%%
  %%% Usage Example
  %%%
  > logi_builtin_sink_process:install(info, self(), [{extra, hoge}]).
  > logi:info("Hello World").
  > flush().
  # TODO: show output
  > logi_builtin_sink_process:uninstall().
```


<a name="types"></a>

## Data Types ##




### <a name="type-dst">dst()</a> ###


<pre><code>
dst() = pid() | port() | (RegName::atom()) | {RegName::atom(), Node::node()}
</code></pre>




### <a name="type-extra">extra()</a> ###


<pre><code>
extra() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#install-2">install/2</a></td><td>Equivalent to <a href="#install-3"><tt>install(Condition, Dst, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#install-3">install/3</a></td><td>Installs a sink.</td></tr><tr><td valign="top"><a href="#uninstall-0">uninstall/0</a></td><td>Equivalent to <a href="#uninstall-1"><tt>uninstall([])</tt></a>.</td></tr><tr><td valign="top"><a href="#uninstall-1">uninstall/1</a></td><td>Uninstalls a sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="install-2"></a>

### install/2 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Dst::<a href="#type-dst">dst()</a>) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>
<br />

Equivalent to [`install(Condition, Dst, [])`](#install-3).

<a name="install-3"></a>

### install/3 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Dst::<a href="#type-dst">dst()</a>, Options) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {id, <a href="logi_sink.md#type-id">logi_sink:id()</a>} | {channel, <a href="logi_channel.md#type-id">logi_channel:id()</a>} | {extra, <a href="#type-extra">extra()</a>} | <a href="logi_channel.md#type-install_sink_option">logi_channel:install_sink_option()</a></code></li></ul>

Installs a sink

When `logi_sink:write/4` is invoked,
the message `{`LOGI_MSG', self(), logi_context:context(), io:format(), [term()], extra()}' will send to `Dst` process.

The default value of `Options`: <br />
- id: `logi_builtin_sink_null` <br />
- channel: `logi_channel:default_channel()` <br />
- extra: `undefined` <br />

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
- id: `logi_builtin_sink_null` <br />
- channel: `logi_channel:default_channel()` <br />

