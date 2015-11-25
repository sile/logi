

# Module logi_sink #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Sinks.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_sink` behaviour.__<br /> Required callback functions: `write/4`.

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




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>

 A module that implements the `logi_sink` behaviour.



### <a name="type-extra_data">extra_data()</a> ###


<pre><code>
extra_data() = term()
</code></pre>

 The value of the fourth arguemnt of the `write/4` callback function.

NOTE:
This value will be loaded from ETS every time the `write/4` is called.
Therefore, very huge data can cause a performance issue.



### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>

 The identifier of a sink.
The sinks installed in the same channel must have different identifiers.



### <a name="type-sink">sink()</a> ###


__abstract datatype__: `sink()`

 A sink instance.



### <a name="type-spec">spec()</a> ###


<pre><code>
spec() = <a href="#type-sink">sink()</a> | <a href="logi_sink_agent.md#type-spec">logi_sink_agent:spec()</a>
</code></pre>




### <a name="type-written_data">written_data()</a> ###


<pre><code>
written_data() = <a href="logi_layout.md#type-formatted_data">logi_layout:formatted_data()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cleanup-2">cleanup/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_extra_data-1">get_extra_data/1</a></td><td>Gets the extra data of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Sink</code></td></tr><tr><td valign="top"><a href="#instantiate-2">instantiate/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_callback_module-1">is_callback_module/1</a></td><td>Returns <code>true</code> if <code>X</code> is a module which implements the <code>sink</code> behaviour, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_sink-1">is_sink/1</a></td><td>Returns <code>true</code> if <code>X</code> is a sink instance, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_spec-1">is_spec/1</a></td><td>Returns <code>true</code> if <code>X</code> is a <code>sink()</code> object, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#write-4">write/4</a></td><td>Writes a log message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cleanup-2"></a>

### cleanup/2 ###

<pre><code>
cleanup(ParentSup, AgentSup) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>ParentSup = <a href="logi_sink_agent.md#type-agent_set_sup">logi_sink_agent:agent_set_sup()</a></code></li><li><code>AgentSup = <a href="logi_sink_agent.md#type-agent_sup">logi_sink_agent:agent_sup()</a> | undefined</code></li></ul>

<a name="get_extra_data-1"></a>

### get_extra_data/1 ###

<pre><code>
get_extra_data(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-extra_data">extra_data()</a>
</code></pre>
<br />

Gets the extra data of `Sink`

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

Gets the module of `Sink`

<a name="instantiate-2"></a>

### instantiate/2 ###

<pre><code>
instantiate(ParentSup, Spec::<a href="#type-spec">spec()</a>) -&gt; {ok, <a href="#type-sink">sink()</a>, AgentSup, AgentPid} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>ParentSup = <a href="logi_sink_agent.md#type-agent_set_sup">logi_sink_agent:agent_set_sup()</a></code></li><li><code>AgentSup = <a href="logi_sink_agent.md#type-agent_sup">logi_sink_agent:agent_sup()</a> | undefined</code></li><li><code>AgentPid = <a href="logi_sink_agent.md#type-agent">logi_sink_agent:agent()</a> | undefined</code></li><li><code>Reason = term()</code></li></ul>

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

<a name="is_spec-1"></a>

### is_spec/1 ###

<pre><code>
is_spec(X::<a href="#type-spec">spec()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a `sink()` object, otherwise `false`

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, ExtraData::<a href="#type-extra_data">extra_data()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Creates a new sink instance

<a name="write-4"></a>

### write/4 ###

<pre><code>
write(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Format::<a href="io.md#type-format">io:format()</a>, Data::<a href="logi_layout.md#type-data">logi_layout:data()</a>, X4::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-written_data">written_data()</a>
</code></pre>
<br />

Writes a log message

If it fails to write, an exception will be raised.

