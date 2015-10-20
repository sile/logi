

# Module logi_builtin_sink_io_device #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A built-in IO device sink.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##

This sink writes log messages to an IO device (e.g. standard output, file, etc)


### <a name="NOTE">NOTE</a> ###

This module is provided for debugging/testing purposes only.
(e.g. Overload protection is missing)


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > application:set_env(logi, warn_no_parse_transform, false). % Suppresses noisy warnings
  %%
  %% 1. The default IO device is <code>standard_io</code>
  %%
  > logi_builtin_sink_io_device:install(info).
  > logi:info("hello world").
  2015-10-21 05:21:52.332 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world
  %%
  %% 2. Outputs to a file
  %%
  > {ok, Fd} = file:open("/tmp/hoge", [write]).
  > logi_builtin_sink_io_device:install(info, [{io_device, Fd}, {if_exists, supersede}]).
  > logi:info("hello world").
  > file:read_file("/tmp/hoge").
  {ok,<<"2015-10-21 05:23:19.940 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world\n">>}
  %%
  %% 3. Customizes message layout
  %%
  > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[my_layout] " ++ Format ++ "\n", Data) end).
  > logi_builtin_sink_io_device:install(info, [{layout, Layout}, {if_exists, supersede}]).
  > logi:info("hello world").
  [my_layout] hello world
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#install-1">install/1</a></td><td>Equivalent to <a href="#install-2"><tt>install(Condition, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#install-2">install/2</a></td><td>Installs a sink.</td></tr><tr><td valign="top"><a href="#uninstall-0">uninstall/0</a></td><td>Equivalent to <a href="#uninstall-1"><tt>uninstall([])</tt></a>.</td></tr><tr><td valign="top"><a href="#uninstall-1">uninstall/1</a></td><td>Uninstalls a sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="install-1"></a>

### install/1 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>
<br />

Equivalent to [`install(Condition, [])`](#install-2).

<a name="install-2"></a>

### install/2 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Options) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {id, <a href="logi_sink.md#type-id">logi_sink:id()</a>} | {channel, <a href="logi_channel.md#type-id">logi_channel:id()</a>} | {io_device, <a href="io.md#type-device">io:device()</a>} | {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>} | <a href="logi_channel.md#type-install_sink_option">logi_channel:install_sink_option()</a></code></li></ul>

Installs a sink

The default value of `Options`: <br />
- id: `logi_builtin_sink_io_device` <br />
- channel: `logi_channel:default_channel()` <br />
- io_device: `standard_io` <br />
- layout: `logi_builtin_layout_simple:new()` <br />

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
- id: `logi_builtin_sink_io_device` <br />
- channel: `logi_channel:default_channel()` <br />

