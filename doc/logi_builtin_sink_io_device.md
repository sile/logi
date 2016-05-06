

# Module logi_builtin_sink_io_device #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A built-in IO device sink.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink_writer`](logi_sink_writer.md).

<a name="description"></a>

## Description ##

This sink writes log messages to an IO device (e.g. standard output, file, etc)

The default layout is `logi_builtin_layout_default:new()`.


### <a name="NOTE">NOTE</a> ###

This module is provided for debugging/testing purposes only.
(e.g. Overload protection is missing)


### <a name="EXAMPLE">EXAMPLE</a> ###

The default IO device is `standard_io`:

```erlang

  > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
  > {ok, _} = logi_channel:install_sink(logi_builtin_sink_io_device:new(foo), info).
  > logi:info("hello world").
  2015-10-21 05:21:52.332 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world
```

Outputs to a file:

```erlang

  > {ok, Fd} = file:open("/tmp/hoge", [write]).
  > Sink = logi_builtin_sink_io_device:new(foo, [{io_device, Fd}]).
  > {ok, _} = logi_channel:install_sink(Sink, info).
  > logi:info("hello world").
  > file:read_file("/tmp/hoge").
  {ok,<<"2015-10-21 05:23:19.940 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world\n">>}
```

Customizes message layout:

```erlang

  > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[my_layout] " ++ Format ++ "\n", Data) end).
  > Sink = logi_builtin_sink_io_device:new(foo, [{layout, Layout}]).
  > {ok, _} = logi_channel:install_sink(Sink, info).
  > logi:info("hello world").
  [my_layout] hello world
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Id, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new sink instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(Id, [])`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Options) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {io_device, <a href="io.md#type-device">io:device()</a>} | {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>}</code></li></ul>

Creates a new sink instance


#### <a name="DEFAULT_VALUE">DEFAULT VALUE</a> ####

- io_device: `standard_io`
- layout: `logi_builtin_layout_default:new()`

