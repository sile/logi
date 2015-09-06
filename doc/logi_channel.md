

# Module logi_channel #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Channel management module.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>




### <a name="type-install_sink_option">install_sink_option()</a> ###


<pre><code>
install_sink_option() = {lifetime, timeout() | pid()} | {if_exists, error | ignored | supersede}
</code></pre>




### <a name="type-install_sink_options">install_sink_options()</a> ###


<pre><code>
install_sink_options() = [<a href="#type-install_sink_option">install_sink_option()</a>]
</code></pre>




### <a name="type-install_sink_result">install_sink_result()</a> ###


<pre><code>
install_sink_result() = {ok, OldSink::undefined | <a href="logi_sink.md#type-sink">logi_sink:sink()</a>} | {error, {already_installed, <a href="logi_sink.md#type-sink">logi_sink:sink()</a>}}
</code></pre>




### <a name="type-uninstall_sink_result">uninstall_sink_result()</a> ###


<pre><code>
uninstall_sink_result() = {ok, <a href="logi_sink.md#type-sink">logi_sink:sink()</a>} | error
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-1">create/1</a></td><td>Creates a new channel.</td></tr><tr><td valign="top"><a href="#default_channel-0">default_channel/0</a></td><td>The default channel.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Deletes a channel.</td></tr><tr><td valign="top"><a href="#find_sink-2">find_sink/2</a></td><td>TODO.</td></tr><tr><td valign="top"><a href="#install_sink-2">install_sink/2</a></td><td>Equivalent to <a href="#install_sink-3"><tt>install_sink(ChannelId, Sink, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#install_sink-3">install_sink/3</a></td><td>Installs a sink.</td></tr><tr><td valign="top"><a href="#set_condition-3">set_condition/3</a></td><td>TODO.</td></tr><tr><td valign="top"><a href="#uninstall_sink-2">uninstall_sink/2</a></td><td>Uninstalls a sink.</td></tr><tr><td valign="top"><a href="#which_channels-0">which_channels/0</a></td><td>Returns a list of all running channels.</td></tr><tr><td valign="top"><a href="#which_sinks-1">which_sinks/1</a></td><td>Returns a list of installed sinks.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create-1"></a>

### create/1 ###

<pre><code>
create(ChannelId::<a href="#type-id">id()</a>) -&gt; ok
</code></pre>
<br />

Creates a new channel

If the channel exists, nothing happens.

<a name="default_channel-0"></a>

### default_channel/0 ###

<pre><code>
default_channel() -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

The default channel

The channel  is created automatically when `logi` application was started.

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(ChannelId::<a href="#type-id">id()</a>) -&gt; ok
</code></pre>
<br />

Deletes a channel

If the channel does not exists, it will be silently ignored.

<a name="find_sink-2"></a>

### find_sink/2 ###

<pre><code>
find_sink(ChannelId::<a href="#type-id">id()</a>, SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; <a href="#type-uninstall_sink_result">uninstall_sink_result()</a>
</code></pre>
<br />

TODO

<a name="install_sink-2"></a>

### install_sink/2 ###

<pre><code>
install_sink(ChannelId::<a href="#type-id">id()</a>, Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; <a href="#type-install_sink_result">install_sink_result()</a>
</code></pre>
<br />

Equivalent to [`install_sink(ChannelId, Sink, [])`](#install_sink-3).

<a name="install_sink-3"></a>

### install_sink/3 ###

<pre><code>
install_sink(ChannelId::<a href="#type-id">id()</a>, Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Options::<a href="#type-install_sink_options">install_sink_options()</a>) -&gt; <a href="#type-install_sink_result">install_sink_result()</a>
</code></pre>
<br />

Installs a sink

<a name="set_condition-3"></a>

### set_condition/3 ###

<pre><code>
set_condition(ChannelId::<a href="#type-id">id()</a>, SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>) -&gt; {ok, <a href="logi_sink.md#type-condition">logi_sink:condition()</a>} | error
</code></pre>
<br />

TODO

<a name="uninstall_sink-2"></a>

### uninstall_sink/2 ###

<pre><code>
uninstall_sink(ChannelId::<a href="#type-id">id()</a>, SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; <a href="#type-uninstall_sink_result">uninstall_sink_result()</a>
</code></pre>
<br />

Uninstalls a sink

<a name="which_channels-0"></a>

### which_channels/0 ###

<pre><code>
which_channels() -&gt; [<a href="#type-id">id()</a>]
</code></pre>
<br />

Returns a list of all running channels

<a name="which_sinks-1"></a>

### which_sinks/1 ###

<pre><code>
which_sinks(ChannelId::<a href="#type-id">id()</a>) -&gt; [<a href="logi_sink.md#type-id">logi_sink:id()</a>]
</code></pre>
<br />

Returns a list of installed sinks

