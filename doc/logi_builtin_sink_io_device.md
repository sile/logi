

# Module logi_builtin_sink_io_device #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A built-in IO device sink.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##

This sink writes log messages to an IO device (e.g. standard output, file, etc)

The default layout is `logi_builtin_layout_simple:new()`.


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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new(standard_io)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new sink instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(standard_io)`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(IoDevice::<a href="io.md#type-device">io:device()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

