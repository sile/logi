

# Module logi_channel #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Log Message Channels.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##

A channel (logically) receives log messages from loggers
and delivers the messages to installed sinks.


### <a name="EXAMPLE">EXAMPLE</a> ###

Basic usage:

```erlang

  > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
  %%
  %% CREATE CHANNEL
  %%
  > ok = logi_channel:create(sample_log).
  > logi_channel:which_channels().
  [sample_log,logi_default_log]  % 'logi_default_log' is created automatically when 'logi' application was started
  %%
  %% INSTALL SINK
  %%
  > WriteFun = fun (_, _, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
  > Sink = logi_builtin_sink_fun:new(WriteFun).
  > {ok, _} = logi_channel:install_sink(info, Sink, [{channel, sample_log}]). % Installs <code>Sink</code> with <code>info</code> level
  > logi_channel:which_sinks([{channel, sample_log}]).
  [logi_builtin_sink_fun]
  %%
  %% OUTPUT LOG MESSAGE
  %%
  > logi:debug("hello world", [], [{logger, sample_log}]).
  % The message is not emitted (the severity is too low).
  > logi:info("hello world", [], [{logger, sample_log}]).
  [my_sink] hello world
  > logi:alert("hello world", [], [{logger, sample_log}]).
  [my_sink] hello world
  > logi:info("hello world"). % If <code>logger</code> option is omitted, the default channel will be used
  % The message is not emitted (no sinks are installed to the default channel).
```

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>

 The identifier of a channel



### <a name="type-install_sink_option">install_sink_option()</a> ###


<pre><code>
install_sink_option() = {id, <a href="logi_sink.md#type-id">logi_sink:id()</a>} | {channel, <a href="#type-id">id()</a>} | {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>} | {lifetime, timeout() | pid()} | {if_exists, error | ignore | supersede}
</code></pre>

 Let `Sink` be the sink which is subject of the installation.

`id`:
- The identifier of `Sink`
- default: `logi_sink:get_module(Sink)`

`channel`:
- The channel in which `Sink` will be installed
- default: `logi_channel:default_channel()`

`layout`:
- The layout of `Sink`.
- This layout will be passed as the second argument of the `SinkModule:write/5` function.
- default: `logi_sink:default_layout(Sink)`

`lifetime`:
- The lifetime of `Sink`.
- When `timeout()` expires or `pid()` exits, the sink will be automatically uninstalled from the channel.
- default: `infinity`

`if_exists`:
- The confliction handling policy.
- If a sink with the same identifier already exists,
  - `error`: the function returns an error `{error, {already_installed, ExistingSink}}`.
  - `ignore`: the new sink is ignored. Then the function returns `{ok, ExistingSink}`.
  - `supersede`: the new sink supersedes it. Then the function returns `{ok, OldSink}`.
- default: `error`



### <a name="type-install_sink_options">install_sink_options()</a> ###


<pre><code>
install_sink_options() = [<a href="#type-install_sink_option">install_sink_option()</a>]
</code></pre>




### <a name="type-install_sink_result">install_sink_result()</a> ###


<pre><code>
install_sink_result() = {ok, OldSink::undefined | <a href="#type-installed_sink">installed_sink()</a>} | {error, {already_installed, <a href="#type-installed_sink">installed_sink()</a>}}
</code></pre>

 The result of [`install_sink/2`](#install_sink-2).

If there does not exist a sink which has the same identifier with a new one,
the function returns `{ok, undefined}`.

Otherwise the result value depends on the value of the `if_exists` option
(see the description of `install_sink_option/0` for details).



### <a name="type-installed_sink">installed_sink()</a> ###


<pre><code>
installed_sink() = {<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>, <a href="logi_sink.md#type-sink">logi_sink:sink()</a>}
</code></pre>

 The information of an installed sink



### <a name="type-uninstall_sink_result">uninstall_sink_result()</a> ###


<pre><code>
uninstall_sink_result() = {ok, <a href="#type-installed_sink">installed_sink()</a>} | error
</code></pre>

 The result of [`uninstall_sink/2`](#uninstall_sink-2).

The function returns `{ok, Sink}` if the specified sink exists in the channel, `error` otherwise.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-1">create/1</a></td><td>Creates a new channel.</td></tr><tr><td valign="top"><a href="#default_channel-0">default_channel/0</a></td><td>The default channel.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Deletes a channel.</td></tr><tr><td valign="top"><a href="#find_sink-1">find_sink/1</a></td><td>Equivalent to <a href="#find_sink-2"><tt>find_sink(SinkId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#find_sink-2">find_sink/2</a></td><td>Searchs for <code>SinkId</code> in <code>Channel</code>; returns <code>{ok, Sink}</code>, or <code>error</code> if <code>SinkId</code> is not present.</td></tr><tr><td valign="top"><a href="#install_sink-2">install_sink/2</a></td><td>Equivalent to <a href="#install_sink-3"><tt>install_sink(Condition, Sink, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#install_sink-3">install_sink/3</a></td><td>Installs <code>Sink</code></td></tr><tr><td valign="top"><a href="#uninstall_sink-1">uninstall_sink/1</a></td><td>Equivalent to <a href="#uninstall_sink-2"><tt>uninstall_sink(SinkId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#uninstall_sink-2">uninstall_sink/2</a></td><td>Uninstalls the sink which has the identifier <code>SinkId</code> from <code>Channel</code></td></tr><tr><td valign="top"><a href="#update_sink-2">update_sink/2</a></td><td>Updates the sink which associated to the identifier <code>SinkId</code></td></tr><tr><td valign="top"><a href="#which_channels-0">which_channels/0</a></td><td>Returns a list of all existing channels.</td></tr><tr><td valign="top"><a href="#which_sinks-0">which_sinks/0</a></td><td>Equivalent to <a href="#which_sinks-1"><tt>which_sinks([])</tt></a>.</td></tr><tr><td valign="top"><a href="#which_sinks-1">which_sinks/1</a></td><td>Returns a list of installed sinks.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create-1"></a>

### create/1 ###

<pre><code>
create(Channel::<a href="#type-id">id()</a>) -&gt; ok
</code></pre>
<br />

Creates a new channel

If the channel already exists, nothing happens.

If there exists a process or a ETS table with the same name as `Channel`, the function crashes.

<a name="default_channel-0"></a>

### default_channel/0 ###

<pre><code>
default_channel() -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

The default channel

This channel is created automatically when `logi` application was started.

NOTE: The default channel ID is the same as the default logger ID ([`logi:default_logger/0`](logi.md#default_logger-0))

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Channel::<a href="#type-id">id()</a>) -&gt; ok
</code></pre>
<br />

Deletes a channel

If the channel does not exists, it is silently ignored.

<a name="find_sink-1"></a>

### find_sink/1 ###

<pre><code>
find_sink(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; {ok, Sink::<a href="#type-installed_sink">installed_sink()</a>} | error
</code></pre>
<br />

Equivalent to [`find_sink(SinkId, [])`](#find_sink-2).

<a name="find_sink-2"></a>

### find_sink/2 ###

<pre><code>
find_sink(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Options) -&gt; {ok, Sink} | error
</code></pre>

<ul class="definitions"><li><code>Options = [{channel, Channel}]</code></li><li><code>Channel = <a href="#type-id">id()</a></code></li><li><code>Sink = <a href="#type-installed_sink">installed_sink()</a></code></li></ul>

Searchs for `SinkId` in `Channel`; returns `{ok, Sink}`, or `error` if `SinkId` is not present

The default value of the `channel` option is `logi_channel:default_channel()`.

<a name="install_sink-2"></a>

### install_sink/2 ###

<pre><code>
install_sink(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; <a href="#type-install_sink_result">install_sink_result()</a>
</code></pre>
<br />

Equivalent to [`install_sink(Condition, Sink, [])`](#install_sink-3).

<a name="install_sink-3"></a>

### install_sink/3 ###

<pre><code>
install_sink(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Options::<a href="#type-install_sink_options">install_sink_options()</a>) -&gt; <a href="#type-install_sink_result">install_sink_result()</a>
</code></pre>
<br />

Installs `Sink`

<a name="uninstall_sink-1"></a>

### uninstall_sink/1 ###

<pre><code>
uninstall_sink(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; <a href="#type-uninstall_sink_result">uninstall_sink_result()</a>
</code></pre>
<br />

Equivalent to [`uninstall_sink(SinkId, [])`](#uninstall_sink-2).

<a name="uninstall_sink-2"></a>

### uninstall_sink/2 ###

<pre><code>
uninstall_sink(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Options) -&gt; <a href="#type-uninstall_sink_result">uninstall_sink_result()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [{channel, Channel}]</code></li><li><code>Channel = <a href="#type-id">id()</a></code></li></ul>

Uninstalls the sink which has the identifier `SinkId` from `Channel`

The default value of the `channel` option is `logi_channel:default_channel()`.

<a name="update_sink-2"></a>

### update_sink/2 ###

<pre><code>
update_sink(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Options) -&gt; {ok, Old} | error
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {channel, <a href="#type-id">id()</a>} | {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>} | {condition, <a href="logi_sink.md#type-condition">logi_sink:condition()</a>} | {sink, <a href="logi_sink.md#type-sink">logi_sink:sink()</a>}</code></li><li><code>Old = <a href="#type-installed_sink">installed_sink()</a></code></li></ul>

Updates the sink which associated to the identifier `SinkId`

This function returns `{ok, Old}` if such a sink exists, `error` otherwise.
`Old` is the old contents of the sink.


#### <a name="OPTIONS">OPTIONS</a> ####

`channel`:
- The channel in which `SinkId` is installed
- default: `logi_channel:default_channel()`

`layout`:
- A new layout
- default: The current layout associated to `SinkId`

`condition`:
- A new condition
- default: The current condition associated to `SinkId`

`sink`:
- A new sink instance
- default: The current sink instance associated to `SinkId`

<a name="which_channels-0"></a>

### which_channels/0 ###

<pre><code>
which_channels() -&gt; [<a href="#type-id">id()</a>]
</code></pre>
<br />

Returns a list of all existing channels

<a name="which_sinks-0"></a>

### which_sinks/0 ###

<pre><code>
which_sinks() -&gt; [<a href="logi_sink.md#type-id">logi_sink:id()</a>]
</code></pre>
<br />

Equivalent to [`which_sinks([])`](#which_sinks-1).

<a name="which_sinks-1"></a>

### which_sinks/1 ###

<pre><code>
which_sinks(Options) -&gt; [<a href="logi_sink.md#type-id">logi_sink:id()</a>]
</code></pre>

<ul class="definitions"><li><code>Options = [{channel, Channel}]</code></li><li><code>Channel = <a href="#type-id">id()</a></code></li></ul>

Returns a list of installed sinks

The default value of the `channel` option is `logi_channel:default_channel()`.

