

# Module logi_sink #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Sinks.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_sink` behaviour.__<br /> Required callback functions: `make_process_spec/2`.

<a name="description"></a>

## Description ##

A sink will consume the log messages sent to the channel which the sink have been installed.

The main purpose of sinks is to write messages to some output devices (e.g. tty, file, socket).


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
  > ok = logi_channel:create(sample_log).
  > WriteFun = fun (_, _, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
  > Sink = logi_builtin_sink_fun:new(WriteFun).
  > {ok, _} = logi_channel:install_sink(info, Sink, [{id, my_sink}, {channel, sample_log}]).
  > logi:info("Hello World", [], [{logger, sample_log}]).
  [my_sink] Hello World  % 'logi_builtin_sink_fun:write/4' was invoked
```

Sinks have an associated layout:

```erlang

  > WriteFun = fun (Context, Layout, Format, Data) -> io:format(logi_layout:format(Context, Format, Data, Layout)) end.
  > Sink = logi_builtin_sink_fun:new(WriteFun).
  > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format("[EXAMPLE] " ++ Format ++"\n", Data) end).
  > {ok, _} = logi_channel:install_sink(info, Sink, [{layout, Layout}]). % Installs <code>Sink</code> to the default channel
  > logi:info("hello world").
  [EXAMPLE]hello world
  %% If 'layout' option is not specified, the result of <code>logi_sink:default_layout(Sink)</code> will be used instead.
  > {ok, _} = logi_channel:install_sink(info, Sink, [{if_exists, supersede}]).
  > logi:info("hello world").
  2015-11-09 22:18:33.934 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world
```

A channel can have multiple sinks:

```erlang

  > ok = logi_channel:create(sample_log).
  > WriteFun_0 = fun (_, _, Format, Data) -> io:format("[sink_0] " ++ Format ++ "\n", Data) end.
  > WriteFun_1 = fun (_, _, Format, Data) -> io:format("[sink_1] " ++ Format ++ "\n", Data) end.
  > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun_0), [{id, sink_0}, {channel, sample_log}]).
  > {ok, _} = logi_channel:install_sink(info, logi_builtin_sink_fun:new(WriteFun_1), [{id, sink_1}, {channel, sample_log}]).
  > logi:info("Hello World", [], [{logger, sample_log}]).
  [sink_0] Hello World
  [sink_1] Hello World
```

<a name="types"></a>

## Data Types ##




### <a name="type-arg">arg()</a> ###


<pre><code>
arg() = term()
</code></pre>




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>




### <a name="type-parent">parent()</a> ###


<pre><code>
parent() = {pid(), <a href="#type-sink_sup">sink_sup()</a>}
</code></pre>




### <a name="type-sink">sink()</a> ###


__abstract datatype__: `sink()`




### <a name="type-sink_sup">sink_sup()</a> ###


<pre><code>
sink_sup() = pid()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_arg-1">get_arg/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_callback_module-1">is_callback_module/1</a></td><td>Returns <code>true</code> if <code>X</code> is a module which implements the <code>sink</code> behaviour, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_sink-1">is_sink/1</a></td><td>Returns <code>true</code> if <code>X</code> is a sink instance, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#make_noop_process_spec-2">make_noop_process_spec/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_root_parent-1">make_root_parent/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_started-2">notify_started/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_stopped-1">notify_stopped/1</a></td><td></td></tr><tr><td valign="top"><a href="#recv_started-1">recv_started/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_child-2">start_child/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop_child-2">stop_child/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_arg-1"></a>

### get_arg/1 ###

<pre><code>
get_arg(Logi_sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-arg">arg()</a>
</code></pre>
<br />

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Logi_sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

<a name="is_callback_module-1"></a>

### is_callback_module/1 ###

<pre><code>
is_callback_module(X::<a href="#type-callback_module">callback_module()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a module which implements the `sink` behaviour, otherwise `false`

<a name="is_sink-1"></a>

### is_sink/1 ###

<pre><code>
is_sink(X::<a href="#type-sink">sink()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a sink instance, otherwise `false`

<a name="make_noop_process_spec-2"></a>

### make_noop_process_spec/2 ###

<pre><code>
make_noop_process_spec(Parent::<a href="#type-parent">parent()</a>, Writer::<a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a>) -&gt; <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>
<br />

<a name="make_root_parent-1"></a>

### make_root_parent/1 ###

<pre><code>
make_root_parent(SinkSup::<a href="#type-sink_sup">sink_sup()</a>) -&gt; <a href="#type-parent">parent()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, Arg::<a href="#type-arg">arg()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

<a name="notify_started-2"></a>

### notify_started/2 ###

<pre><code>
notify_started(X1::<a href="#type-parent">parent()</a>, Writer::<a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a>) -&gt; ok
</code></pre>
<br />

<a name="notify_stopped-1"></a>

### notify_stopped/1 ###

<pre><code>
notify_stopped(X1::<a href="#type-parent">parent()</a>) -&gt; ok
</code></pre>
<br />

<a name="recv_started-1"></a>

### recv_started/1 ###

<pre><code>
recv_started(SinkSup::<a href="#type-sink_sup">sink_sup()</a>) -&gt; <a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a>
</code></pre>
<br />

<a name="start_child-2"></a>

### start_child/2 ###

<pre><code>
start_child(X1::<a href="#type-parent">parent()</a>, Sink::<a href="#type-sink">sink()</a>) -&gt; {ok, <a href="#type-sink_sup">sink_sup()</a>} | {error, Reason::term()}
</code></pre>
<br />

<a name="stop_child-2"></a>

### stop_child/2 ###

<pre><code>
stop_child(X1::<a href="#type-parent">parent()</a>, SinkSup::<a href="#type-sink_sup">sink_sup()</a>) -&gt; ok
</code></pre>
<br />

