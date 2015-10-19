

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


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > ok = logi_channel:create(sample_log).
  > WriteFun = fun (_, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
  > Sink = logi_sink:new(my_sink, logi_builtin_sink_fun, info, WriteFun).
  > {ok, _} = logi_channel:install_sink(sample_log, Sink).
  > logi:info("Hello World", [], [{logger, sample_log}]).
  [my_sink] Hello World  % 'logi_builtin_sink_fun:write/4' was invoked
```

Conventionally, sink implementation modules provide `install` function to install the sink.

```erlang

  > ok = logi_channel:create(sample_log).
  > WriteFun = fun (_, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
  > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun, [{channel, sample_log}]).
  > logi:info("Hello World", [], [{logger, sample_log}]).
  [my_sink] Hello World  % 'logi_builtin_sink_fun:write/4' was invoked
```

A channel can have multiple sinks.

```erlang

  > ok = logi_channel:create(sample_log).
  > WriteFun_0 = fun (_, Format, Data) -> io:format("[sink_0] " ++ Format ++ "\n", Data) end.
  > WriteFun_1 = fun (_, Format, Data) -> io:format("[sink_1] " ++ Format ++ "\n", Data) end.
  > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun_0, [{id, sink_0}, {channel, sample_log}]).
  > {ok, _} = logi_builtin_sink_fun:install(info, WriteFun_1, [{id, sink_1}, {channel, sample_log}]).
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

  > logi_sink:new(Id, Module, #{application => stdlib}).                          % application
  > logi_sink:new(Id, Module, #{application => [stdlib, kernel]}).                % applications
  > logi_sink:new(Id, Module, #{module => lists}).                                % module
  > logi_sink:new(Id, Module, #{module => [lists, dict]}).                        % modules
  > logi_sink:new(Id, Module, #{application => kernel, module => [lists, dict]}). % application and modules
  > logi_sink:new(Id, Module, #{severity => [info, alert], module => lists}).     % severity and module
```



### <a name="type-map_form">map_form()</a> ###


<pre><code>
map_form() = #{id =&gt; <a href="#type-id">id()</a>, module =&gt; <a href="#type-callback_module">callback_module()</a>, condition =&gt; <a href="#type-condition">condition()</a>, extra_data =&gt; <a href="#type-extra_data">extra_data()</a>}
</code></pre>

 The map representation of a sink



### <a name="type-normalized_condition">normalized_condition()</a> ###


<pre><code>
normalized_condition() = [<a href="logi.md#type-severity">logi:severity()</a> | {<a href="logi.md#type-severity">logi:severity()</a>, <a href="logi_location.md#type-application">logi_location:application()</a>} | {<a href="logi.md#type-severity">logi:severity()</a>, <a href="logi_location.md#type-application">logi_location:application()</a>, module()}]
</code></pre>

 The normalized form of a `condition/0`.

```erlang

  > Normalize = fun (C) -> lists:sort(logi_sink:get_normalized_condition(logi_sink:new(null, logi_builtin_sink_null, C))) end.
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

  > logi_sink:new(Id, Module, info).          % level
  > logi_sink:new(Id, Module, {info, alert}). % range
  > logi_sink:new(Id, Module, [info, alert]). % list
```



### <a name="type-sink">sink()</a> ###


__abstract datatype__: `sink()`

 A sink.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Creates a new sink from <code>Map</code></td></tr><tr><td valign="top"><a href="#get_condition-1">get_condition/1</a></td><td>Gets the condition of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_extra_data-1">get_extra_data/1</a></td><td>Gets the extra data of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td>Gets the ID of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Sink</code></td></tr><tr><td valign="top"><a href="#get_normalized_condition-1">get_normalized_condition/1</a></td><td>Gets the normalized condition of <code>Sink</code></td></tr><tr><td valign="top"><a href="#is_callback_module-1">is_callback_module/1</a></td><td>Returns <code>true</code> if <code>X</code> is a module which implements the <code>sink</code> behaviour, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_condition-1">is_condition/1</a></td><td>Returns <code>true</code> if <code>X</code> is a valid <code>condition()</code> value, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_sink-1">is_sink/1</a></td><td>Returns <code>true</code> if <code>X</code> is a sink, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Module, Module)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(Id, Module, debug)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Equivalent to <a href="#new-4"><tt>new(Id, Module, Condition, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Creates a new sink.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>Converts <code>Sink</code> into a map form.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-map_form">map_form()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Creates a new sink from `Map`

Default Value:
- id: the value of `module`
- module: none (mandatory)
- condition: `debug`
- extra_data: `undefined`

```erlang

  > logi_sink:to_map(logi_sink:from_map(#{module => logi_builtin_sink_null})).
  #{condition => debug,
    extra_data => undefined,
    id => logi_builtin_sink_null,
    module => logi_builtin_sink_null}
```

<a name="get_condition-1"></a>

### get_condition/1 ###

<pre><code>
get_condition(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-condition">condition()</a>
</code></pre>
<br />

Gets the condition of `Sink`

<a name="get_extra_data-1"></a>

### get_extra_data/1 ###

<pre><code>
get_extra_data(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-extra_data">extra_data()</a>
</code></pre>
<br />

Gets the extra data of `Sink`

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Gets the ID of `Sink`

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

Gets the module of `Sink`

<a name="get_normalized_condition-1"></a>

### get_normalized_condition/1 ###

<pre><code>
get_normalized_condition(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-normalized_condition">normalized_condition()</a>
</code></pre>
<br />

Gets the normalized condition of `Sink`

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

Equivalent to [`new(Module, Module)`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="#type-id">id()</a>, Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Equivalent to [`new(Id, Module, debug)`](#new-3).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Id::<a href="#type-id">id()</a>, Module::<a href="#type-callback_module">callback_module()</a>, Condition::<a href="#type-condition">condition()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Equivalent to [`new(Id, Module, Condition, undefined)`](#new-4).

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(Id::<a href="#type-id">id()</a>, Module::<a href="#type-callback_module">callback_module()</a>, Condition::<a href="#type-condition">condition()</a>, ExtraData::<a href="#type-extra_data">extra_data()</a>) -&gt; <a href="#type-sink">sink()</a>
</code></pre>
<br />

Creates a new sink

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Sink::<a href="#type-sink">sink()</a>) -&gt; <a href="#type-map_form">map_form()</a>
</code></pre>
<br />

Converts `Sink` into a map form

