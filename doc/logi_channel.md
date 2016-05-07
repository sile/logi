

# Module logi_channel #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Log Message Channels.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

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
  > WriteFun = fun (_, Format, Data) -> io:format("[my_sink] " ++ Format ++ "\n", Data) end.
  > Sink = logi_builtin_sink_fun:new(sample_sink, WriteFun).
  > {ok, _} = logi_channel:install_sink(sample_log, Sink, info). % Installs <code>Sink</code> with <code>info</code> level
  > logi_channel:which_sinks(sample_log).
  [sample_sink]
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
install_sink_option() = {if_exists, error | ignore | supersede}
</code></pre>

`if_exists`:
- The confliction handling policy.
- If a sink with the same identifier already exists,
  - `error`: the function returns an error `{error, {already_installed, ExistingSink}}`.
  - `ignore`: the new sink is ignored. Then the function returns `{ok, ExistingSink}`.
  - `supersede`: the new sink supersedes it. Then the function returns `{ok, OldSink}`.
- default: `supersede`



### <a name="type-install_sink_options">install_sink_options()</a> ###


<pre><code>
install_sink_options() = [<a href="#type-install_sink_option">install_sink_option()</a>]
</code></pre>




### <a name="type-installed_sink">installed_sink()</a> ###


<pre><code>
installed_sink() = #{sink =&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>, condition =&gt; <a href="logi_condition.md#type-condition">logi_condition:condition()</a>, sink_sup =&gt; <a href="logi_sink_proc.md#type-sink_sup">logi_sink_proc:sink_sup()</a>, writer =&gt; <a href="logi_sink_writer.md#type-writer">logi_sink_writer:writer()</a> | undefined}
</code></pre>

 The information of an installed sink

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-1">create/1</a></td><td>Creates a new channel.</td></tr><tr><td valign="top"><a href="#default_channel-0">default_channel/0</a></td><td>The default channel.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Deletes a channel.</td></tr><tr><td valign="top"><a href="#find_sink-1">find_sink/1</a></td><td>Equivalent to <a href="#find_sink-2"><tt>find_sink(default_channel(), SinkId)</tt></a>.</td></tr><tr><td valign="top"><a href="#find_sink-2">find_sink/2</a></td><td>Searchs for <code>SinkId</code> in <code>Channel</code></td></tr><tr><td valign="top"><a href="#install_sink-2">install_sink/2</a></td><td>Equivalent to <a href="#install_sink-3"><tt>install_sink(default_channel(), Sink, Condition)</tt></a>.</td></tr><tr><td valign="top"><a href="#install_sink-3">install_sink/3</a></td><td>Equivalent to <a href="#install_sink_opt-4"><tt>install_sink_opt(Channel, Sink, Condition, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#install_sink_opt-3">install_sink_opt/3</a></td><td>Equivalent to <a href="#install_sink_opt-4"><tt>install_sink_opt(default_channel(), Sink, Condition,
Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#install_sink_opt-4">install_sink_opt/4</a></td><td>Installs <code>Sink</code></td></tr><tr><td valign="top"><a href="#set_sink_condition-2">set_sink_condition/2</a></td><td>Equivalent to <a href="#set_sink_condition-3"><tt>set_sink_condition(default_channel(), SinkId, Condition)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_sink_condition-3">set_sink_condition/3</a></td><td>Sets the applicable condition of the <code>SinkId</code></td></tr><tr><td valign="top"><a href="#uninstall_sink-1">uninstall_sink/1</a></td><td>Equivalent to <a href="#uninstall_sink-2"><tt>uninstall_sink(default_channel(), SinkId)</tt></a>.</td></tr><tr><td valign="top"><a href="#uninstall_sink-2">uninstall_sink/2</a></td><td>Uninstalls the sink which has the identifier <code>SinkId</code> from <code>Channel</code></td></tr><tr><td valign="top"><a href="#whereis_sink_proc-1">whereis_sink_proc/1</a></td><td>Equivalent to <a href="#whereis_sink_proc-2"><tt>whereis_sink_proc(default_channel(), Path)</tt></a>.</td></tr><tr><td valign="top"><a href="#whereis_sink_proc-2">whereis_sink_proc/2</a></td><td>Returns the pid associated with <code>Path</code></td></tr><tr><td valign="top"><a href="#which_channels-0">which_channels/0</a></td><td>Returns a list of all existing channels.</td></tr><tr><td valign="top"><a href="#which_sinks-0">which_sinks/0</a></td><td>Equivalent to <a href="#which_sinks-1"><tt>which_sinks(default_channel())</tt></a>.</td></tr><tr><td valign="top"><a href="#which_sinks-1">which_sinks/1</a></td><td>Returns a list of installed sinks.</td></tr></table>


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

Equivalent to [`find_sink(default_channel(), SinkId)`](#find_sink-2).

<a name="find_sink-2"></a>

### find_sink/2 ###

<pre><code>
find_sink(Channel::<a href="#type-id">id()</a>, SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; {ok, Sink::<a href="#type-installed_sink">installed_sink()</a>} | error
</code></pre>
<br />

Searchs for `SinkId` in `Channel`

The function  returns `{ok, Sink}`, or `error` if `SinkId` is not present

<a name="install_sink-2"></a>

### install_sink/2 ###

<pre><code>
install_sink(Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Condition::<a href="logi_condition.md#type-condition">logi_condition:condition()</a>) -&gt; {ok, Old} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Old = undefined | <a href="#type-installed_sink">installed_sink()</a></code></li><li><code>Reason = {cannot_start, term()}</code></li></ul>

Equivalent to [`install_sink(default_channel(), Sink, Condition)`](#install_sink-3).

<a name="install_sink-3"></a>

### install_sink/3 ###

<pre><code>
install_sink(Channel::<a href="#type-id">id()</a>, Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Condition::<a href="logi_condition.md#type-condition">logi_condition:condition()</a>) -&gt; {ok, Old} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Old = undefined | <a href="#type-installed_sink">installed_sink()</a></code></li><li><code>Reason = {cannot_start, term()}</code></li></ul>

Equivalent to [`install_sink_opt(Channel, Sink, Condition, [])`](#install_sink_opt-4).

<a name="install_sink_opt-3"></a>

### install_sink_opt/3 ###

<pre><code>
install_sink_opt(Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Condition::<a href="logi_condition.md#type-condition">logi_condition:condition()</a>, Options::<a href="#type-install_sink_options">install_sink_options()</a>) -&gt; {ok, Old} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Old = undefined | <a href="#type-installed_sink">installed_sink()</a></code></li><li><code>Reason = {already_installed, <a href="#type-installed_sink">installed_sink()</a>} | {cannot_start, term()}</code></li></ul>

Equivalent to [`install_sink_opt(default_channel(), Sink, Condition,Options)`](#install_sink_opt-4).

<a name="install_sink_opt-4"></a>

### install_sink_opt/4 ###

<pre><code>
install_sink_opt(Channel::<a href="#type-id">id()</a>, Sink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Condition::<a href="logi_condition.md#type-condition">logi_condition:condition()</a>, Options::<a href="#type-install_sink_options">install_sink_options()</a>) -&gt; {ok, Old} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Old = undefined | <a href="#type-installed_sink">installed_sink()</a></code></li><li><code>Reason = {already_installed, <a href="#type-installed_sink">installed_sink()</a>} | {cannot_start, term()}</code></li></ul>

Installs `Sink`

If failed to start a sink process specified by `logi_sink:get_spec(Sink)`,
the function returns `{cannot_start, FailureReason}`.

If there does not exist a sink which has the same identifier with a new one,
the function returns `{ok, undefined}`.

Otherwise the result value depends on the value of the `if_exists` option
(see the description of `install_sink_option/0` for details).

<a name="set_sink_condition-2"></a>

### set_sink_condition/2 ###

<pre><code>
set_sink_condition(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Condition::<a href="logi_condition.md#type-condition">logi_condition:condition()</a>) -&gt; {ok, Old::<a href="logi_condition.md#type-condition">logi_condition:condition()</a>} | error
</code></pre>
<br />

Equivalent to [`set_sink_condition(default_channel(), SinkId, Condition)`](#set_sink_condition-3).

<a name="set_sink_condition-3"></a>

### set_sink_condition/3 ###

<pre><code>
set_sink_condition(Channel::<a href="#type-id">id()</a>, SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Condition::<a href="logi_condition.md#type-condition">logi_condition:condition()</a>) -&gt; {ok, Old} | error
</code></pre>

<ul class="definitions"><li><code>Old = <a href="logi_condition.md#type-condition">logi_condition:condition()</a></code></li></ul>

Sets the applicable condition of the `SinkId`

The function returns `{ok, Old}` if the specified sink exists in the channel, `error` otherwise.

<a name="uninstall_sink-1"></a>

### uninstall_sink/1 ###

<pre><code>
uninstall_sink(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; {ok, <a href="#type-installed_sink">installed_sink()</a>} | error
</code></pre>
<br />

Equivalent to [`uninstall_sink(default_channel(), SinkId)`](#uninstall_sink-2).

<a name="uninstall_sink-2"></a>

### uninstall_sink/2 ###

<pre><code>
uninstall_sink(Channel::<a href="#type-id">id()</a>, SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; {ok, Sink::<a href="#type-installed_sink">installed_sink()</a>} | error
</code></pre>
<br />

Uninstalls the sink which has the identifier `SinkId` from `Channel`

The function returns `{ok, Sink}` if the specified sink exists in the channel, `error` otherwise.

<a name="whereis_sink_proc-1"></a>

### whereis_sink_proc/1 ###

<pre><code>
whereis_sink_proc(Path::[<a href="logi_sink.md#type-id">logi_sink:id()</a>]) -&gt; pid() | undefined
</code></pre>
<br />

Equivalent to [`whereis_sink_proc(default_channel(), Path)`](#whereis_sink_proc-2).

<a name="whereis_sink_proc-2"></a>

### whereis_sink_proc/2 ###

<pre><code>
whereis_sink_proc(Channel::<a href="#type-id">id()</a>, Path::[<a href="logi_sink.md#type-id">logi_sink:id()</a>]) -&gt; pid() | undefined
</code></pre>
<br />

Returns the pid associated with `Path`

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

Equivalent to [`which_sinks(default_channel())`](#which_sinks-1).

<a name="which_sinks-1"></a>

### which_sinks/1 ###

<pre><code>
which_sinks(Channel::<a href="#type-id">id()</a>) -&gt; [<a href="logi_sink.md#type-id">logi_sink:id()</a>]
</code></pre>
<br />

Returns a list of installed sinks

