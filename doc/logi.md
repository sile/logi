

# Module logi #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Logger Interface.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##

This module mainly provides logger related functions.
A logger has own headers, metadata, filter and can issue log messages to a destination channel.


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > logi_builtin_sink_io_device:install(info). % Installs a sink to the default channel
  > logi:log(info, "hello world", [], []). % The log message is (logically) sent to the channel and consumed by the sink
  2015-10-22 13:16:37.003 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world
```

<a name="types"></a>

## Data Types ##




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = #{}
</code></pre>

 The headers of a log message.

Headers are intended to be included in the outputs written by sinks.



### <a name="type-log_option">log_option()</a> ###


<pre><code>
log_option() = {logger, <a href="#type-logger">logger()</a>} | {location, <a href="logi_location.md#type-location">logi_location:location()</a>} | {headers, <a href="#type-headers">headers()</a>} | {metadata, <a href="#type-metadata">metadata()</a>} | {timestamp, <a href="erlang.md#type-timestamp">erlang:timestamp()</a>}
</code></pre>

 [logger]
- The logger of interest
- Default: `logi:default_logger()`

[locatoin]
- The log message issued location
- Default: `logi_location:guess_location()`
- see also

[headers]
- The headers of the log message
- They are merged with the headers of the logger (the former has priority when key collisions occur)
- Default: `#{}`

[metadata]
- The metadata of the log message
- They are merged with the metadata of the logger (the former has priority when key collisions occur)
- Default: `#{}`

[timestamp]
- The log message issued time
- Default: `os:timestamp()`



### <a name="type-log_options">log_options()</a> ###


<pre><code>
log_options() = [<a href="#type-log_option">log_option()</a>]
</code></pre>




### <a name="type-logger">logger()</a> ###


<pre><code>
logger() = <a href="#type-logger_id">logger_id()</a> | <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

 A logger



### <a name="type-logger_id">logger_id()</a> ###


<pre><code>
logger_id() = atom()
</code></pre>

 The ID of a saved logger instance (see: [`save/2`](#save-2)).

If such a logger instance does not exist,
the ID will be regarded as an alias of the expression `new([{channel, LoggerId}])`.



### <a name="type-logger_instance">logger_instance()</a> ###


__abstract datatype__: `logger_instance()`

 A logger instance



### <a name="type-logger_map_form">logger_map_form()</a> ###


<pre><code>
logger_map_form() = #{channel =&gt; <a href="logi_channel.md#type-id">logi_channel:id()</a>, headers =&gt; <a href="#type-headers">headers()</a>, metadata =&gt; <a href="#type-metadata">metadata()</a>, filter =&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>, next =&gt; <a href="#type-logger_instance">logger_instance()</a>}
</code></pre>

 The map representation of a logger.

`filter` and `next` fields are optional
(e.g. If a logger has no filter, the `filter` field is omitted from the corresponding map).



### <a name="type-metadata">metadata()</a> ###


<pre><code>
metadata() = #{}
</code></pre>

 The metadata of a log message

Metadata are not intended to be included directly in the outputs written by sinks.
The main purpose of metadata is to provide means to convey information from the log issuer to filters or sinks.



### <a name="type-new_option">new_option()</a> ###


<pre><code>
new_option() = {channel, <a href="logi_channel.md#type-id">logi_channel:id()</a>} | {headers, <a href="#type-headers">headers()</a>} | {metadata, <a href="#type-metadata">metadata()</a>} | {filter, <a href="logi_filter.md#type-filter">logi_filter:filter()</a>} | {next, <a href="#type-logger_instance">logger_instance()</a>}
</code></pre>

 [channel]
- The destination channel
- The log messages issued by the created logger will (logically) send to the channel
- Default: `logi_channel:default_channel()`

[headers]
- The headers of the created logger
- Default: `#{}`

[metadata]
- The metadata of the created logger
- Default: `#{}`

[filter]
- A log message filter
- Default: none (optional)

[next]
- A next logger
- An application of the some function (e.g. [`log/4`](#log-4)) to the created logger is also applied to the next logger
- Default: none (optional)



### <a name="type-new_options">new_options()</a> ###


<pre><code>
new_options() = [<a href="#type-new_option">new_option()</a>]
</code></pre>




### <a name="type-severity">severity()</a> ###


<pre><code>
severity() = debug | verbose | info | notice | warning | error | critical | alert | emergency
</code></pre>

 Severity of a log message

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alert-1">alert/1</a></td><td>Equivalent to <a href="#alert-2"><tt>alert(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#alert-2">alert/2</a></td><td>Equivalent to <a href="#alert-3"><tt>alert(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#alert-3">alert/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(alert, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#critical-1">critical/1</a></td><td>Equivalent to <a href="#critical-2"><tt>critical(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#critical-2">critical/2</a></td><td>Equivalent to <a href="#critical-3"><tt>critical(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#critical-3">critical/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(critical, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#debug-1">debug/1</a></td><td>Equivalent to <a href="#debug-2"><tt>debug(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#debug-2">debug/2</a></td><td>Equivalent to <a href="#debug-3"><tt>debug(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#debug-3">debug/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(debug, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#default_logger-0">default_logger/0</a></td><td>Returns the default logger.</td></tr><tr><td valign="top"><a href="#delete_headers-1">delete_headers/1</a></td><td>Equivalent to <a href="#delete_headers-2"><tt>delete_headers(Keys, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#delete_headers-2">delete_headers/2</a></td><td>Deletes headers which associated with <code>Keys</code></td></tr><tr><td valign="top"><a href="#delete_metadata-1">delete_metadata/1</a></td><td>Equivalent to <a href="#delete_metadata-2"><tt>delete_metadata(Keys, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#delete_metadata-2">delete_metadata/2</a></td><td>Deletes metadata entries which associated with <code>Keys</code></td></tr><tr><td valign="top"><a href="#emergency-1">emergency/1</a></td><td>Equivalent to <a href="#emergency-2"><tt>emergency(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#emergency-2">emergency/2</a></td><td>Equivalent to <a href="#emergency-3"><tt>emergency(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#emergency-3">emergency/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(emergency, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#erase-0">erase/0</a></td><td>Returns the saved loggers and deletes them from the process dictionary.</td></tr><tr><td valign="top"><a href="#erase-1">erase/1</a></td><td>Returns the logger associated with <code>LoggerId</code> and deletes it from the process dictionary.</td></tr><tr><td valign="top"><a href="#error-1">error/1</a></td><td>Equivalent to <a href="#error-2"><tt>error(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#error-2">error/2</a></td><td>Equivalent to <a href="#error-3"><tt>error(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(error, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>Aggregates <code>Loggers</code> into a logger instance.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Creates a new logger instance from <code>Map</code></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Equivalent to <a href="#info-2"><tt>info(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Equivalent to <a href="#info-3"><tt>info(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(info, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#is_logger-1">is_logger/1</a></td><td>Returns <code>true</code> if <code>X</code> is a logger, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_severity-1">is_severity/1</a></td><td>Returns <code>true</code> if <code>X</code> is a severity, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td>Loads a logger which associated with the ID <code>LoggerId</code> from the process dictionary.</td></tr><tr><td valign="top"><a href="#load_default-0">load_default/0</a></td><td>Equivalent to <a href="#load-1"><tt>load(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#log-4">log/4</a></td><td>Issues a log message to the destination channel.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new([])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new logger instance.</td></tr><tr><td valign="top"><a href="#notice-1">notice/1</a></td><td>Equivalent to <a href="#notice-2"><tt>notice(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#notice-2">notice/2</a></td><td>Equivalent to <a href="#notice-3"><tt>notice(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#notice-3">notice/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(notice, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#save-2">save/2</a></td><td>Saves <code>Logger</code> with the ID <code>LoggerId</code> to the process dictionary.</td></tr><tr><td valign="top"><a href="#save_as_default-1">save_as_default/1</a></td><td>Equivalent to <a href="#save-2"><tt>save(default_logger(), Logger)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_headers-1">set_headers/1</a></td><td>Equivalent to <a href="#set_headers-2"><tt>set_headers(Headers, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#set_headers-2">set_headers/2</a></td><td>Sets headers of the logger.</td></tr><tr><td valign="top"><a href="#set_metadata-1">set_metadata/1</a></td><td>Equivalent to <a href="#set_metadata-2"><tt>set_metadata(Metadata, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#set_metadata-2">set_metadata/2</a></td><td>Sets metadata of the logger.</td></tr><tr><td valign="top"><a href="#severities-0">severities/0</a></td><td>Returns the available severity list.</td></tr><tr><td valign="top"><a href="#severity_level-1">severity_level/1</a></td><td>Returns the level of <code>Severity</code></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Flattens the nested logger.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>Converts <code>Logger</code> into a map form.</td></tr><tr><td valign="top"><a href="#verbose-1">verbose/1</a></td><td>Equivalent to <a href="#verbose-2"><tt>verbose(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#verbose-2">verbose/2</a></td><td>Equivalent to <a href="#verbose-3"><tt>verbose(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#verbose-3">verbose/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(verbose, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#warning-1">warning/1</a></td><td>Equivalent to <a href="#warning-2"><tt>warning(Format, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#warning-2">warning/2</a></td><td>Equivalent to <a href="#warning-3"><tt>warning(Format, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#warning-3">warning/3</a></td><td>Equivalent to <a href="#log-4"><tt>log(warning, Format, Data, Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#which_loggers-0">which_loggers/0</a></td><td>Returns the ID list of the saved loggers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alert-1"></a>

### alert/1 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`alert(Format, [])`](#alert-2).

<a name="alert-2"></a>

### alert/2 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`alert(Format, Data, [])`](#alert-3).

<a name="alert-3"></a>

### alert/3 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(alert, Format, Data, Options)`](#log-4).

<a name="critical-1"></a>

### critical/1 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`critical(Format, [])`](#critical-2).

<a name="critical-2"></a>

### critical/2 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`critical(Format, Data, [])`](#critical-3).

<a name="critical-3"></a>

### critical/3 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(critical, Format, Data, Options)`](#log-4).

<a name="debug-1"></a>

### debug/1 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`debug(Format, [])`](#debug-2).

<a name="debug-2"></a>

### debug/2 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`debug(Format, Data, [])`](#debug-3).

<a name="debug-3"></a>

### debug/3 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(debug, Format, Data, Options)`](#log-4).

<a name="default_logger-0"></a>

### default_logger/0 ###

<pre><code>
default_logger() -&gt; <a href="#type-logger_id">logger_id()</a>
</code></pre>
<br />

Returns the default logger

The default channel [`logi_channel:default_channel/0`](logi_channel.md#default_channel-0) which corresponds to the logger
is started automatically when `logi` application was started.

<a name="delete_headers-1"></a>

### delete_headers/1 ###

<pre><code>
delete_headers(Keys::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`delete_headers(Keys, [])`](#delete_headers-2).

<a name="delete_headers-2"></a>

### delete_headers/2 ###

<pre><code>
delete_headers(Keys::[term()], Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>}</code></li></ul>

Deletes headers which associated with `Keys`

If the logger has nested loggers, the function is applied to them recursively.


#### <a name="OPTION">OPTION</a> ####

[logger]
- The logger to which the operation applies.
- Default: `logi:default_logger()`.

<a name="delete_metadata-1"></a>

### delete_metadata/1 ###

<pre><code>
delete_metadata(Keys::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`delete_metadata(Keys, [])`](#delete_metadata-2).

<a name="delete_metadata-2"></a>

### delete_metadata/2 ###

<pre><code>
delete_metadata(Keys::[term()], Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>}</code></li></ul>

Deletes metadata entries which associated with `Keys`

If the logger has nested loggers, the function is applied to them recursively.


#### <a name="OPTION">OPTION</a> ####

[logger]
- The logger to which the operation applies.
- Default: `logi:default_logger()`.

<a name="emergency-1"></a>

### emergency/1 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`emergency(Format, [])`](#emergency-2).

<a name="emergency-2"></a>

### emergency/2 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`emergency(Format, Data, [])`](#emergency-3).

<a name="emergency-3"></a>

### emergency/3 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(emergency, Format, Data, Options)`](#log-4).

<a name="erase-0"></a>

### erase/0 ###

<pre><code>
erase() -&gt; [{<a href="#type-logger_id">logger_id()</a>, <a href="#type-logger_instance">logger_instance()</a>}]
</code></pre>
<br />

Returns the saved loggers and deletes them from the process dictionary.

<a name="erase-1"></a>

### erase/1 ###

<pre><code>
erase(LoggerId::<a href="#type-logger_id">logger_id()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a> | undefined
</code></pre>
<br />

Returns the logger associated with `LoggerId` and deletes it from the process dictionary.

Returns `undefined` if no logger is associated with `LoggerId`.

<a name="error-1"></a>

### error/1 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`error(Format, [])`](#error-2).

<a name="error-2"></a>

### error/2 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`error(Format, Data, [])`](#error-3).

<a name="error-3"></a>

### error/3 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(error, Format, Data, Options)`](#log-4).

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(Loggers::[<a href="#type-logger">logger()</a>]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Aggregates `Loggers` into a logger instance

The head logger in `Loggers` becomes the root of the aggregation.

e.g. `from_list([new(), new(), new()])` is equivalent to `new([{next, new([{next, new()}])}])`.

```erlang

  > GetChannel = fun (Logger) -> maps:get(channel, logi:to_map(Logger)) end.
  > Logger0 = logi:new([{channel, aaa}]).
  > Logger1 = logi:new([{channel, bbb}]).
  > Logger2 = logi:new([{channel, ccc}, {next, logi:new([{channel, ccc_sub}])}]).
  > [aaa, bbb] = lists:map(GetChannel, logi:to_list(logi:from_list([Logger0, Logger1]))).
  > [ccc, ccc_sub, aaa, bbb] = lists:map(GetChannel, logi:to_list(logi:from_list([Logger2, Logger0, Logger1]))).
```

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-logger_map_form">logger_map_form()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Creates a new logger instance from `Map`

Default Value:
- channel: `logi_channel:default_channel()`
- headers: `#{}`
- metadata: `#{}`
- filter: none (optional)
- next: none (optional)

```erlang

  > logi:to_map(logi:from_map(#{})).
  #{channel => logi_default_log,headers => #{},metadata => #{}}
```

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`info(Format, [])`](#info-2).

<a name="info-2"></a>

### info/2 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`info(Format, Data, [])`](#info-3).

<a name="info-3"></a>

### info/3 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(info, Format, Data, Options)`](#log-4).

<a name="is_logger-1"></a>

### is_logger/1 ###

<pre><code>
is_logger(X::<a href="#type-logger">logger()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a logger, otherwise `false`

<a name="is_severity-1"></a>

### is_severity/1 ###

<pre><code>
is_severity(X::<a href="#type-severity">severity()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a severity, otherwise `false`

<a name="load-1"></a>

### load/1 ###

<pre><code>
load(LoggerId::<a href="#type-logger_id">logger_id()</a>) -&gt; {ok, <a href="#type-logger_instance">logger_instance()</a>} | error
</code></pre>
<br />

Loads a logger which associated with the ID `LoggerId` from the process dictionary

```erlang

  > error = logi:load(hoge_log).
  > logi:save(hoge_log, logi:new()).
  > {ok, _} = logi:load(hoge_log).
```

<a name="load_default-0"></a>

### load_default/0 ###

<pre><code>
load_default() -&gt; {ok, <a href="#type-logger_instance">logger_instance()</a>} | error
</code></pre>
<br />

Equivalent to [`load(default_logger())`](#load-1).

<a name="log-4"></a>

### log/4 ###

<pre><code>
log(Severity::<a href="#type-severity">severity()</a>, Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Issues a log message to the destination channel.

If the logger has a filter, the message will be passed to it.
And if the message has not been discarded by a filter,
the logger will (logically) send it to the destination channel.
Finally, the message will be consumed by the sinks which are installed to the channel.
But the sinks which does not satisfy specified condition (i.e. `logi_sink:condition/0`) are ignored.

```erlang

  > logi_builtin_sink_io_device:install(info). % Installs to the default channel
  > logi:log(debug, "hello world", [], []). % There are no applicable sinks (the severity is too low)
  > logi:log(info, "hello world", [], []). % The log message is consumed by the above sink
  2015-10-22 13:16:37.003 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [] hello world
```

If the logger has nested loggers, the function is applied to them recursively.

```erlang

  > logi_builtin_sink_io_device:install(info). % Installs to the default channel
  > Logger = logi:from_list([logi:new([{headers, #{id => hoge}}]), logi:new([{headers, #{id => fuga}}])]).
  > logi:log(info, "hello world", [], [{logger, Logger}]).
  2015-10-22 13:28:10.332 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [id=hoge] hello world
  2015-10-22 13:28:10.332 [info] nonode@nohost <0.91.0> erl_eval:do_apply:673 [id=fuga] hello world
```


#### <a name="NOTE">NOTE</a> ####

Typically, it is preferred to log messages through the wrapper functions (i.e. `logi:Severity/{1,2,3}`)
rather than calling the function directly.

If the `{parse_transform, logi_transform}` compiler option is specified,
the invocation of the wrapper functions will be transformed to a more efficient code at compile time.

For example `logi:info("hello world)` will be transformed to a code such as following:

```erlang

  %% Current location (<code>Application</code>, <code>Module</code>, <code>Function</code> and <code>Line</code>) is detected at compile time
  Location = logi_location:unsafe_new(self(), Application, Module, Function, Line),
  case logi:'_ready'(info, Location, Options) of
    {Logger, []}                    -> Logger;
    {Logger, ListOfContextAndSinks} -> logi:<code>_write</code>(ListOfContextAndSinks, Format, Data)
  end.
```

From the efficiency point of view, the following two points are important:
- 1. An implicit call of [`logi_location:guess_location/0`](logi_location.md#guess_location-0) to guess the currrent location is replaced by the more efficient and accurate code
- 2. If it is unnecessary (e.g. there are no applicable sinks), `Format` and `Data` will not be evaluated

The [`logi_location:guess_location/0`](logi_location.md#guess_location-0) is a heavy function,
so if it is called at runtime, a warning will be emitted via the `error_logger` module.

__See also:__ [logi_location:guess_location/0](logi_location.md#guess_location-0).

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`new([])`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Options::<a href="#type-new_options">new_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Creates a new logger instance

<a name="notice-1"></a>

### notice/1 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`notice(Format, [])`](#notice-2).

<a name="notice-2"></a>

### notice/2 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`notice(Format, Data, [])`](#notice-3).

<a name="notice-3"></a>

### notice/3 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(notice, Format, Data, Options)`](#log-4).

<a name="save-2"></a>

### save/2 ###

<pre><code>
save(LoggerId::<a href="#type-logger_id">logger_id()</a>, Logger::<a href="#type-logger">logger()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a> | undefined
</code></pre>
<br />

Saves `Logger` with the ID `LoggerId` to the process dictionary

If `LoggerId` already exists, the old logger instance is deleted and replaced by `Logger`
and the function returns the old instance.
Otherwise it returns `undefined`.

In the process, a saved logger instance can be referred by the ID.

```erlang

  > Logger = logi:new().
  > logi:save(sample_log, Logger).
  % The following two expression is equivalent.
  > logi:info("hello world", [{logger, Logger}]).  % referred by instance
  > logi:info("hello world", [{logger, sample_log}]). % referred by ID
```

<a name="save_as_default-1"></a>

### save_as_default/1 ###

<pre><code>
save_as_default(Logger::<a href="#type-logger">logger()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a> | undefined
</code></pre>
<br />

Equivalent to [`save(default_logger(), Logger)`](#save-2).

<a name="set_headers-1"></a>

### set_headers/1 ###

<pre><code>
set_headers(Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`set_headers(Headers, [])`](#set_headers-2).

<a name="set_headers-2"></a>

### set_headers/2 ###

<pre><code>
set_headers(Headers::<a href="#type-headers">headers()</a>, Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>} | {if_exists, ignore | overwrite | supersede}</code></li></ul>

Sets headers of the logger

If the logger has nested loggers, the function is applied to them recursively.


#### <a name="OPTION">OPTION</a> ####

[logger]
- The logger to which the operation applies.
- Default: `logi:default_logger()`.

[if_exists]
- If the value is `supersede`, the existing headers are deleted and replaced by `Headers`.
- If the value is `overwrite`, the existing headers and `Headers` are merged and the rear has priority when a key collision occurs.
- If the value is `ignore`, the existing headers and `Headers` are merged and the former has priority when a key collision occurs.
- Default: `overwrite`

```erlang

  > Logger = logi:new([{headers, #{a => 10, b => 20}}]).
  > Set = fun (Headers, IfExists) ->
              L = logi:set_headers(Headers, [{logger, Logger}, {if_exists, IfExists}]),
              maps:get(headers, logi:to_map(L))
          end.
  > true = #{a => 0,           c => 30} =:= Set(#{a => 0, c => 30}, supersede).
  > true = #{a => 0,  b => 20, c => 30} =:= Set(#{a => 0, c => 30}, overwrite).
  > true = #{a => 10, b => 20, c => 30} =:= Set(#{a => 0, c => 30}, ignore).
```

<a name="set_metadata-1"></a>

### set_metadata/1 ###

<pre><code>
set_metadata(Metadata::<a href="#type-metadata">metadata()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`set_metadata(Metadata, [])`](#set_metadata-2).

<a name="set_metadata-2"></a>

### set_metadata/2 ###

<pre><code>
set_metadata(Metadata::<a href="#type-metadata">metadata()</a>, Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>} | {if_exists, ignore | overwrite | supersede}</code></li></ul>

Sets metadata of the logger

If the logger has nested loggers, the function is applied to them recursively.


#### <a name="OPTION">OPTION</a> ####

[logger]
- The logger to which the operation applies.
- Default: `logi:default_logger()`.

[if_exists]
- If the value is `supersede`, the existing metadata is deleted and replaced by `Metadata`.
- If the value is `overwrite`, the existing metadata and `Metadata` are merged and the rear has priority when a key collision occurs.
- If the value is `ignore`, the existing metadata and `Metadata` are merged and the former has priority when a key collision occurs.
- Default: `overwrite`

```erlang

  > Logger = logi:new([{metadata, #{a => 10, b => 20}}]).
  > Set = fun (Metadata, IfExists) ->
              L = logi:set_metadata(Metadata, [{logger, Logger}, {if_exists, IfExists}]),
              maps:get(metadata, logi:to_map(L))
          end.
  > true = #{a => 0,           c => 30} =:= Set(#{a => 0, c => 30}, supersede).
  > true = #{a => 0,  b => 20, c => 30} =:= Set(#{a => 0, c => 30}, overwrite).
  > true = #{a => 10, b => 20, c => 30} =:= Set(#{a => 0, c => 30}, ignore).
```

<a name="severities-0"></a>

### severities/0 ###

<pre><code>
severities() -&gt; [<a href="#type-severity">severity()</a>]
</code></pre>
<br />

Returns the available severity list

The list are ordered by the their severity level (see: [`severity_level/1`](#severity_level-1)).

<a name="severity_level-1"></a>

### severity_level/1 ###

<pre><code>
severity_level(Severity::<a href="#type-severity">severity()</a>) -&gt; 1..9
</code></pre>
<br />

Returns the level of `Severity`

The higher the severity is, the lower the level is.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Logger::<a href="#type-logger">logger()</a>) -&gt; [<a href="#type-logger_instance">logger_instance()</a>]
</code></pre>
<br />

Flattens the nested logger

The nested loggers are collected as a flat list.
The `next` fields of the resulting loggers are removed.

```erlang

  > Logger0 = logi:new().
  > Logger1 = logi:new([{next, Logger0}]).
  > Logger2 = logi:new([{next, Logger1}]).
  > [Logger0] = logi:to_list(Logger0).
  > [Logger0, Logger0] = logi:to_list(Logger1).
  > [Logger0, Logger0, Logger0] = logi:to_list(Logger2).
```

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Logger::<a href="#type-logger">logger()</a>) -&gt; <a href="#type-logger_map_form">logger_map_form()</a>
</code></pre>
<br />

Converts `Logger` into a map form

The optional entries (i.e. `filter` and `next`) will be omitted from the resulting map if the value is not set.

```erlang

  > logi:to_map(logi:new()).
  #{channel => logi_default_log,headers => #{},metadata => #{}}
  > logi:to_map(logi:new([{next, logi:new()}])).
  #{channel => logi_default_log,
    headers => #{},
    metadata => #{},
    next => {logi_logger,logi_default_log,#{},#{},undefined,undefined}}
```

<a name="verbose-1"></a>

### verbose/1 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`verbose(Format, [])`](#verbose-2).

<a name="verbose-2"></a>

### verbose/2 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`verbose(Format, Data, [])`](#verbose-3).

<a name="verbose-3"></a>

### verbose/3 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(verbose, Format, Data, Options)`](#log-4).

<a name="warning-1"></a>

### warning/1 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`warning(Format, [])`](#warning-2).

<a name="warning-2"></a>

### warning/2 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`warning(Format, Data, [])`](#warning-3).

<a name="warning-3"></a>

### warning/3 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>, Data::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`log(warning, Format, Data, Options)`](#log-4).

<a name="which_loggers-0"></a>

### which_loggers/0 ###

<pre><code>
which_loggers() -&gt; [<a href="#type-logger_id">logger_id()</a>]
</code></pre>
<br />

Returns the ID list of the saved loggers

