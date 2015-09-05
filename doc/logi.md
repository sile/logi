

# Module logi #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Logger Interface.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-channel_id">channel_id()</a> ###


<pre><code>
channel_id() = atom()
</code></pre>




### <a name="type-context_handler">context_handler()</a> ###


<pre><code>
context_handler() = {module(), term()}
</code></pre>




### <a name="type-frequency_controller">frequency_controller()</a> ###


<pre><code>
frequency_controller() = term()
</code></pre>




### <a name="type-frequency_spec">frequency_spec()</a> ###


<pre><code>
frequency_spec() = todo
</code></pre>




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = <a href="maps.md#type-map">maps:map</a>(<a href="#type-key">key()</a>, term())
</code></pre>




### <a name="type-key">key()</a> ###


<pre><code>
key() = atom()
</code></pre>




### <a name="type-log_level">log_level()</a> ###


<pre><code>
log_level() = debug | verbose | info | notice | warning | error | critical | alert | emergency
</code></pre>




### <a name="type-log_options">log_options()</a> ###


<pre><code>
log_options() = #{logger =&gt; <a href="#type-logger">logger()</a>, location =&gt; <a href="logi_location.md#type-location">logi_location:location()</a>, headers =&gt; <a href="#type-headers">headers()</a>, metadata =&gt; <a href="#type-metadata">metadata()</a>, frequency =&gt; <a href="#type-frequency_spec">frequency_spec()</a>}
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




### <a name="type-logger_map">logger_map()</a> ###


<pre><code>
logger_map() = #{channel_id =&gt; <a href="logi_channel.md#type-id">logi_channel:id()</a>, headers =&gt; <a href="#type-headers">headers()</a>, metadata =&gt; <a href="#type-metadata">metadata()</a>, context_handler =&gt; <a href="#type-context_handler">context_handler()</a>, frequency_controller =&gt; <a href="#type-frequency_controller">frequency_controller()</a>}
</code></pre>




### <a name="type-metadata">metadata()</a> ###


<pre><code>
metadata() = <a href="maps.md#type-map">maps:map</a>(<a href="#type-key">key()</a>, term())
</code></pre>




### <a name="type-new_option">new_option()</a> ###


<pre><code>
new_option() = {headers, <a href="#type-headers">headers()</a>} | {metadata, <a href="#type-metadata">metadata()</a>} | {context_handler, <a href="#type-context_handler">context_handler()</a>} | {frequency_controller, <a href="#type-frequency_controller">frequency_controller()</a>}
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_logger-0">default_logger/0</a></td><td>Returns the default logger.</td></tr><tr><td valign="top"><a href="#delete_headers-1">delete_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete_headers-2">delete_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_metadata-1">delete_metadata/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete_metadata-2">delete_metadata/2</a></td><td></td></tr><tr><td valign="top"><a href="#erase-1">erase/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_logger-1">is_logger/1</a></td><td></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td></td></tr><tr><td valign="top"><a href="#load_or_new-1">load_or_new/1</a></td><td></td></tr><tr><td valign="top"><a href="#load_or_new-2">load_or_new/2</a></td><td></td></tr><tr><td valign="top"><a href="#load_or_new-3">load_or_new/3</a></td><td></td></tr><tr><td valign="top"><a href="#log-4">log/4</a></td><td></td></tr><tr><td valign="top"><a href="#log_levels-0">log_levels/0</a></td><td>Returns the available log level list.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#save-2">save/2</a></td><td></td></tr><tr><td valign="top"><a href="#save_as_default-1">save_as_default/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_headers-1">set_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_headers-2">set_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_metadata-1">set_metadata/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_metadata-2">set_metadata/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#which_loggers-0">which_loggers/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

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
delete_headers(Keys::[<a href="#type-key">key()</a>]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="delete_headers-2"></a>

### delete_headers/2 ###

<pre><code>
delete_headers(Keys::[<a href="#type-key">key()</a>], Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>}</code></li></ul>

<a name="delete_metadata-1"></a>

### delete_metadata/1 ###

<pre><code>
delete_metadata(Keys::[<a href="#type-key">key()</a>]) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="delete_metadata-2"></a>

### delete_metadata/2 ###

<pre><code>
delete_metadata(Keys::[<a href="#type-key">key()</a>], Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>}</code></li></ul>

<a name="erase-1"></a>

### erase/1 ###

<pre><code>
erase(LoggerId::<a href="#type-logger_id">logger_id()</a>) -&gt; undefined | <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-logger_map">logger_map()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="is_logger-1"></a>

### is_logger/1 ###

<pre><code>
is_logger(X::<a href="#type-logger">logger()</a> | term()) -&gt; boolean()
</code></pre>
<br />

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

<a name="load_or_new-2"></a>

### load_or_new/2 ###

<pre><code>
load_or_new(LoggerId::<a href="#type-logger_id">logger_id()</a>, ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="load_or_new-3"></a>

### load_or_new/3 ###

<pre><code>
load_or_new(LoggerId::<a href="#type-logger_id">logger_id()</a>, ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>, Options::<a href="#type-new_options">new_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="log-4"></a>

### log/4 ###

<pre><code>
log(Severity::<a href="#type-severity">severity()</a>, Format::<a href="io.md#type-format">io:format()</a>, FormatArgs::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; no_return()
</code></pre>
<br />

<a name="log_levels-0"></a>

### log_levels/0 ###

<pre><code>
log_levels() -&gt; [<a href="#type-log_level">log_level()</a>]
</code></pre>
<br />

Returns the available log level list

The log levels are ordered by the severity (The lowest severity level will appear first).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(ChannelId::<a href="logi_channel.md#type-id">logi_channel:id()</a>, Options::<a href="#type-new_options">new_options()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="save-2"></a>

### save/2 ###

<pre><code>
save(LoggerId::<a href="#type-logger_id">logger_id()</a>, Logger::<a href="#type-logger">logger()</a>) -&gt; undefined | <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="save_as_default-1"></a>

### save_as_default/1 ###

<pre><code>
save_as_default(Logger::<a href="#type-logger">logger()</a>) -&gt; undefined | <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="set_headers-1"></a>

### set_headers/1 ###

<pre><code>
set_headers(Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>
<br />

<a name="set_headers-2"></a>

### set_headers/2 ###

<pre><code>
set_headers(Headers::<a href="#type-headers">headers()</a>, Options) -&gt; <a href="#type-logger_instance">logger_instance()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>} | {if_exists, ignore | overwrite | supersede}</code></li></ul>

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

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {logger, <a href="#type-logger">logger()</a>} | {if_exists, ignore | overwrite | supersede}</code></li></ul>

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Logger::<a href="#type-logger">logger()</a>) -&gt; <a href="#type-logger_map">logger_map()</a>
</code></pre>
<br />

<a name="which_loggers-0"></a>

### which_loggers/0 ###

<pre><code>
which_loggers() -&gt; [<a href="#type-logger_id">logger_id()</a>]
</code></pre>
<br />

