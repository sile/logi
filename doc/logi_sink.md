

# Module logi_sink #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Sinks.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_sink` behaviour.__<br /> Required callback functions: `write/5`, `default_layout/1`.

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



### <a name="type-condition">condition()</a> ###


<pre><code>
condition() = <a href="#type-severity_condition">severity_condition()</a> | <a href="#type-location_condition">location_condition()</a>
</code></pre>

 The condition to determine which messages to be consumed by a sink.



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



### <a name="type-location_condition">location_condition()</a> ###


<pre><code>
location_condition() = #{severity =&gt; <a href="#type-severity_condition">severity_condition()</a>, application =&gt; <a href="logi_location.md#type-application">logi_location:application()</a> | [<a href="logi_location.md#type-application">logi_location:application()</a>], module =&gt; module() | [module()]}
</code></pre>

 The messages which satisfy `severity` (default is `debug`) and are sent from the specified location will be consumed.

The location is specified by `application` and `module` (OR condition).

NOTE: The modules which does not belong to any application are forbidden.


#### <a name="EXAMPLE">EXAMPLE</a> ####


```erlang

  > logi_sink:is_condition(#{application => stdlib}).                          % application
  > logi_sink:is_condition(#{application => [stdlib, kernel]}).                % applications
  > logi_sink:is_condition(#{module => lists}).                                % module
  > logi_sink:is_condition(#{module => [lists, dict]}).                        % modules
  > logi_sink:is_condition(#{application => kernel, module => [lists, dict]}). % application and modules
  > logi_sink:is_condition(#{severity => [info, alert], module => lists}).     % severity and module
```



### <a name="type-normalized_condition">normalized_condition()</a> ###


<pre><code>
normalized_condition() = [<a href="logi.md#type-severity">logi:severity()</a> | {<a href="logi.md#type-severity">logi:severity()</a>, <a href="logi_location.md#type-application">logi_location:application()</a>} | {<a href="logi.md#type-severity">logi:severity()</a>, <a href="logi_location.md#type-application">logi_location:application()</a>, module()}]
</code></pre>

 The normalized form of a `condition/0`.

```erlang

  > Normalize = fun (C) -> lists:sort(logi_sink:normalize_condition(C)) end.
  > Normalize(info).
  [alert,critical,emergency,error,info,notice,warning]
  > Normalize({info, alert}).
  [alert,critical,error,info,notice,warning]
  > Normalize(#{severity => [info], application => [kernel, stdlib]}).
  [{info,kernel},{info,stdlib}]
  > Normalize(#{severity => [info], module => [lists, logi]}).
  [{info,logi,logi},{info,stdlib,lists}]
  > Normalize(#{severity => [info], application => kernel, module => [lists, logi]}).
  [{info,kernel},{info,logi,logi},{info,stdlib,lists}]
```



### <a name="type-severity_condition">severity_condition()</a> ###


<pre><code>
severity_condition() = (Min::<a href="logi.md#type-severity">logi:severity()</a>) | {Min::<a href="logi.md#type-severity">logi:severity()</a>, Max::<a href="logi.md#type-severity">logi:severity()</a>} | (Severities::[<a href="logi.md#type-severity">logi:severity()</a>])
</code></pre>

`Min`:
- The messages with `Min` or higher severity will be consumed.

`{Min, Max}`:
- The messages with severity between `Min` and `Max` will be consumed.

`Severities`:
- The messages with severity included in `Severities` will be consumed.


#### <a name="EXAMPLE">EXAMPLE</a> ####


```erlang

  > [emergency,alert]     = logi_sink:normalize_condition(alert).               % level
  > [warning,notice,info] = logi_sink:normalize_condition({info, warning}).      % range
  > [alert,debug,info]    = logi_sink:normalize_condition([debug, info, alert]). % list
```



### <a name="type-sink">sink()</a> ###


__abstract datatype__: `sink()`

 A sink instance.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_layout-1">default_layout/1</a></td><td>Returns the default layout of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_extra_data-1">get_extra_data/1</a></td><td>Gets the extra data of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Sink</code></td></tr><tr><td valign="top"><a href="#is_callback_module-1">is_callback_module/1</a></td><td>Returns <code>true</code> if <code>X</code> is a module which implements the <code>sink</code> behaviour, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_condition-1">is_condition/1</a></td><td>Returns <code>true</code> if <code>X</code> is a valid <code>condition()</code> value, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_sink-1">is_sink/1</a></td><td>Returns <code>true</code> if <code>X</code> is a sink, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Module, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#normalize_condition-1">normalize_condition/1</a></td><td>Returns a normalized form of <code>Condition</code></td></tr><tr><td valign="top"><a href="#unsafe_new-2">unsafe_new/2</a></td><td>Creates a new sink instance without validating the arguments.</td></tr><tr><td valign="top"><a href="#write-5">write/5</a></td><td>Writes a log message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_layout-1"></a>

### default_layout/1 ###

<pre><code>
default_layout(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Returns the default layout of `Sink`

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

<a name="is_callback_module-1"></a>

### is_callback_module/1 ###

<pre><code>
is_callback_module(X::<a href="#type-callback_module">callback_module()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a module which implements the `sink` behaviour, otherwise `false`

<a name="is_condition-1"></a>

### is_condition/1 ###

<pre><code>
is_condition(X::<a href="#type-condition">condition()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a valid `condition()` value, otherwise `false`

<a name="is_sink-1"></a>

### is_sink/1 ###

<pre><code>
is_sink(X::<a href="#type-sink">sink()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a sink, otherwise `false`

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Equivalent to [`new(Module, undefined)`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, ExtraData::<a href="#type-extra_data">extra_data()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Creates a new sink instance

<a name="normalize_condition-1"></a>

### normalize_condition/1 ###

<pre><code>
normalize_condition(Condition::<a href="#type-condition">condition()</a>) -&gt; <a href="#type-normalized_condition">normalized_condition()</a>
</code></pre>
<br />

Returns a normalized form of `Condition`

<a name="unsafe_new-2"></a>

### unsafe_new/2 ###

<pre><code>
unsafe_new(Module::<a href="#type-callback_module">callback_module()</a>, ExtraData::<a href="#type-extra_data">extra_data()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Creates a new sink instance without validating the arguments

<a name="write-5"></a>

### write/5 ###

<pre><code>
write(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Layout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>, Format::<a href="io.md#type-format">io:format()</a>, Data::<a href="logi_layout.md#type-data">logi_layout:data()</a>, X5::<a href="#type-sink">sink()</a>) -&gt; any()
</code></pre>
<br />

Writes a log message

If it fails to write, an exception will be raised.

