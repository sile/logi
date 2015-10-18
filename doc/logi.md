

# Module logi #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Logger Interface.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = #{}
</code></pre>




### <a name="type-log_level">log_level()</a> ###


<pre><code>
log_level() = debug | verbose | info | notice | warning | error | critical | alert | emergency
</code></pre>




### <a name="type-log_option">log_option()</a> ###


<pre><code>
log_option() = {logger, <a href="#type-logger">logger()</a>} | {location, <a href="logi_location.md#type-location">logi_location:location()</a>} | {subject, term()} | {headers, <a href="#type-headers">headers()</a>} | {metadata, <a href="#type-metadata">metadata()</a>} | {timestamp, <a href="erlang.md#type-timestamp">erlang:timestamp()</a>}
</code></pre>




### <a name="type-log_options">log_options()</a> ###


<pre><code>
log_options() = [<a href="#type-log_option">log_option()</a>]
</code></pre>




### <a name="type-logger">logger()</a> ###


<pre><code>
logger() = <a href="#type-logger_id">logger_id()</a> | <a href="#type-logger_instance">logger_instance()</a>
</code></pre>




### <a name="type-logger_id">logger_id()</a> ###


<pre><code>
logger_id() = atom()
</code></pre>




### <a name="type-logger_instance">logger_instance()</a> ###


__abstract datatype__: `logger_instance()`




### <a name="type-logger_map_form">logger_map_form()</a> ###


<pre><code>
logger_map_form() = #{channel_id =&gt; <a href="logi_channel.md#type-id">logi_channel:id()</a>, headers =&gt; <a href="#type-headers">headers()</a>, metadata =&gt; <a href="#type-metadata">metadata()</a>, filters =&gt; [<a href="logi_filter.md#type-filter">logi_filter:filter()</a>]}
</code></pre>




### <a name="type-metadata">metadata()</a> ###


<pre><code>
metadata() = #{}
</code></pre>




### <a name="type-new_option">new_option()</a> ###


<pre><code>
new_option() = {headers, <a href="#type-headers">headers()</a>} | {metadata, <a href="#type-metadata">metadata()</a>} | {filter, <a href="logi_filter.md#type-filter">logi_filter:filter()</a>} | {next, <a href="#type-logger_instance">logger_instance()</a>}
</code></pre>




### <a name="type-new_options">new_options()</a> ###


<pre><code>
new_options() = [<a href="#type-new_option">new_option()</a>]
</code></pre>




### <a name="type-severity">severity()</a> ###


<pre><code>
severity() = <a href="#type-log_level">log_level()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alert-1">alert/1</a></td><td></td></tr><tr><td valign="top"><a href="#alert-2">alert/2</a></td><td></td></tr><tr><td valign="top"><a href="#alert-3">alert/3</a></td><td></td></tr><tr><td valign="top"><a href="#critical-1">critical/1</a></td><td></td></tr><tr><td valign="top"><a href="#critical-2">critical/2</a></td><td></td></tr><tr><td valign="top"><a href="#critical-3">critical/3</a></td><td></td></tr><tr><td valign="top"><a href="#debug-1">debug/1</a></td><td></td></tr><tr><td valign="top"><a href="#debug-2">debug/2</a></td><td></td></tr><tr><td valign="top"><a href="#debug-3">debug/3</a></td><td></td></tr><tr><td valign="top"><a href="#default_logger-0">default_logger/0</a></td><td>Returns the default logger.</td></tr><tr><td valign="top"><a href="#delete_headers-1">delete_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete_headers-2">delete_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_metadata-1">delete_metadata/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete_metadata-2">delete_metadata/2</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-1">emergency/1</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-2">emergency/2</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-3">emergency/3</a></td><td></td></tr><tr><td valign="top"><a href="#erase-0">erase/0</a></td><td></td></tr><tr><td valign="top"><a href="#erase-1">erase/1</a></td><td></td></tr><tr><td valign="top"><a href="#error-1">error/1</a></td><td></td></tr><tr><td valign="top"><a href="#error-2">error/2</a></td><td></td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Creates a new logger instance from <code>Map</code></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td></td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_logger-1">is_logger/1</a></td><td>Returns <code>true</code> if <code>X</code> is a logger, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#is_severity-1">is_severity/1</a></td><td>Returns <code>true</code> if <code>X</code> is a severity, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td></td></tr><tr><td valign="top"><a href="#load_or_new-1">load_or_new/1</a></td><td>Equivalent to <a href="#load_or_new-2"><tt>load_or_new(LoggerId, LoggerId)</tt></a>.</td></tr><tr><td valign="top"><a href="#load_or_new-2">load_or_new/2</a></td><td>Equivalent to <a href="#load_or_new-3"><tt>load_or_new(LoggerId, ChannelId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#load_or_new-3">load_or_new/3</a></td><td></td></tr><tr><td valign="top"><a href="#log-4">log/4</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(ChannelId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new logger instance.</td></tr><tr><td valign="top"><a href="#notice-1">notice/1</a></td><td></td></tr><tr><td valign="top"><a href="#notice-2">notice/2</a></td><td></td></tr><tr><td valign="top"><a href="#notice-3">notice/3</a></td><td></td></tr><tr><td valign="top"><a href="#save-2">save/2</a></td><td></td></tr><tr><td valign="top"><a href="#save_as_default-1">save_as_default/1</a></td><td>Equivalent to <a href="#save-2"><tt>save(default_logger(), Logger)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_headers-1">set_headers/1</a></td><td>Equivalent to <a href="#set_headers-2"><tt>set_headers(Headers, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#set_headers-2">set_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_metadata-1">set_metadata/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_metadata-2">set_metadata/2</a></td><td></td></tr><tr><td valign="top"><a href="#severities-0">severities/0</a></td><td>Returns the available severity list.</td></tr><tr><td valign="top"><a href="#severity_level-1">severity_level/1</a></td><td>Returns the level of <code>Severity</code></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>Converts <code>Logger</code> into a map form.</td></tr><tr><td valign="top"><a href="#verbose-1">verbose/1</a></td><td></td></tr><tr><td valign="top"><a href="#verbose-2">verbose/2</a></td><td></td></tr><tr><td valign="top"><a href="#verbose-3">verbose/3</a></td><td></td></tr><tr><td valign="top"><a href="#warning-1">warning/1</a></td><td></td></tr><tr><td valign="top"><a href="#warning-2">warning/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning-3">warning/3</a></td><td></td></tr><tr><td valign="top"><a href="#which_loggers-0">which_loggers/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alert-1"></a>

### alert/1 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="alert-2"></a>

### alert/2 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="alert-3"></a>

### alert/3 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="critical-1"></a>

### critical/1 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="critical-2"></a>

### critical/2 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="critical-3"></a>

### critical/3 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="debug-1"></a>

### debug/1 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="debug-2"></a>

### debug/2 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="debug-3"></a>

### debug/3 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="default_logger-0"></a>

### default_logger/0 ###

<pre><code>
default_logger() -&gt; <a href="#type-logger_id">logger_id()</a>
</code></pre>
<br />

Returns the default logger

The default logger is started automatically when `logi` application was started.

<a name="delete_headers-1"></a>

### delete_headers/1 ###

<pre><code>
delete_headers(Keys::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="delete_headers-2"></a>

### delete_headers/2 ###

<pre><code>
delete_headers(Keys::[term()], Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>} | {recursive, boolean()}</code></li></ul>

<a name="delete_metadata-1"></a>

### delete_metadata/1 ###

<pre><code>
delete_metadata(Keys::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="delete_metadata-2"></a>

### delete_metadata/2 ###

<pre><code>
delete_metadata(Keys::[term()], Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>}</code></li></ul>

<a name="emergency-1"></a>

### emergency/1 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="emergency-2"></a>

### emergency/2 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="emergency-3"></a>

### emergency/3 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="erase-0"></a>

### erase/0 ###

<pre><code>
erase() -&gt; [{<a href="#type-logger_id">logger_id()</a>, <a href="#type-logger_instance">logger_instance()</a>}]
</code></pre>
<br />

<a name="erase-1"></a>

### erase/1 ###

<pre><code>
erase(LoggerId::<a href="#type-logger_id">logger_id()</a>) -&gt; Old::<a href="#type-logger_instance">logger_instance()</a> | undefined
</code></pre>
<br />

<a name="error-1"></a>

### error/1 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="error-2"></a>

### error/2 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="error-3"></a>

### error/3 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(Loggers::[<a href="#type-logger">logger()</a>]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-logger_map_form">logger_map_form()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Creates a new logger instance from `Map`

Default Value:
- channel_id: none (mandatory)
- headers: `#{}`
- metadata: `#{}`
- filter: none (optional)
- next: none (optional)

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="info-2"></a>

### info/2 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="info-3"></a>

### info/3 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

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

<a name="load_or_new-1"></a>

### load_or_new/1 ###

<pre><code>
load_or_new(LoggerId::<a href="#type-logger_id">logger_id()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`load_or_new(LoggerId, LoggerId)`](#load_or_new-2).

<a name="load_or_new-2"></a>

### load_or_new/2 ###

<pre><code>
load_or_new(LoggerId::<a href="#type-logger_id">logger_id()</a>, ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`load_or_new(LoggerId, ChannelId, [])`](#load_or_new-3).

<a name="load_or_new-3"></a>

### load_or_new/3 ###

<pre><code>
load_or_new(LoggerId::<a href="#type-logger_id">logger_id()</a>, ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>, Options::<a href="#type-new_options">new_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="log-4"></a>

### log/4 ###

<pre><code>
log(Severity::<a href="#type-severity">severity()</a>, Format::<a href="io.md#type-format">io:format()</a>, FormatArgs::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

__This function is deprecated:__ TODO

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`new(default_logger())`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Equivalent to [`new(ChannelId, [])`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>, Options::<a href="#type-new_options">new_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

Creates a new logger instance

<a name="notice-1"></a>

### notice/1 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="notice-2"></a>

### notice/2 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="notice-3"></a>

### notice/3 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="save-2"></a>

### save/2 ###

<pre><code>
save(LoggerId::<a href="#type-logger_id">logger_id()</a>, Logger0::<a href="#type-logger">logger()</a>) -&gt; Old::<a href="#type-logger_instance">logger_instance()</a> | undefined
</code></pre>
<br />

<a name="save_as_default-1"></a>

### save_as_default/1 ###

<pre><code>
save_as_default(Logger::<a href="#type-logger">logger()</a>) -&gt; Old::<a href="#type-logger_instance">logger_instance()</a> | undefined
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

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>} | {if_exists, ignore | overwrite | supersede} | {recursive, boolean()}</code></li></ul>

<a name="set_metadata-1"></a>

### set_metadata/1 ###

<pre><code>
set_metadata(Metadata::<a href="#type-metadata">metadata()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="set_metadata-2"></a>

### set_metadata/2 ###

<pre><code>
set_metadata(Metadata::<a href="#type-metadata">metadata()</a>, Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>} | {if_exists, ignore | overwrite | supersede} | {recursive, boolean()}</code></li></ul>

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

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Logger::<a href="#type-logger">logger()</a>) -&gt; <a href="#type-logger_map_form">logger_map_form()</a>
</code></pre>
<br />

Converts `Logger` into a map form

The entries which has default values will be omitted from the resulting map

<a name="verbose-1"></a>

### verbose/1 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="verbose-2"></a>

### verbose/2 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="verbose-3"></a>

### verbose/3 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="warning-1"></a>

### warning/1 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="warning-2"></a>

### warning/2 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="warning-3"></a>

### warning/3 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="which_loggers-0"></a>

### which_loggers/0 ###

<pre><code>
which_loggers() -&gt; [<a href="#type-logger_id">logger_id()</a>]
</code></pre>
<br />

