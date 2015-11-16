

# Module logi_agent #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-mfargs">mfargs()</a> ###


<pre><code>
mfargs() = {Module::module(), Function::atom(), Args::[term()]}
</code></pre>




### <a name="type-proc_ref">proc_ref()</a> ###


<pre><code>
proc_ref() = pid() | atom() | {global, term()} | {via, module(), term()} | {<a href="#type-resolve_fun">resolve_fun()</a>, term()}
</code></pre>




### <a name="type-resolve_fun">resolve_fun()</a> ###


<pre><code>
resolve_fun() = fun((Name::term()) -&gt; pid() | undefined)
</code></pre>




### <a name="type-shutdown">shutdown()</a> ###


<pre><code>
shutdown() = timeout() | brutal_kill
</code></pre>




### <a name="type-spec">spec()</a> ###


__abstract datatype__: `spec()`




### <a name="type-start_result">start_result()</a> ###


<pre><code>
start_result() = {ok, pid()} | {ok, pid(), <a href="logi_sink.md#type-extra_data">logi_sink:extra_data()</a>} | {error, term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_restart-1">get_restart/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_shutdown-1">get_shutdown/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_start-1">get_start/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_spec-1">is_spec/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#new_external-3">new_external/3</a></td><td></td></tr><tr><td valign="top"><a href="#new_opaque-1">new_opaque/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_agent-2">start_agent/2</a></td><td></td></tr><tr><td valign="top"><a href="#whereis_name-1">whereis_name/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_restart-1"></a>

### get_restart/1 ###

<pre><code>
get_restart(Agent_spec::<a href="#type-spec">spec()</a>) -&gt; <a href="logi_restart_strategy.md#type-strategy">logi_restart_strategy:strategy()</a>
</code></pre>
<br />

<a name="get_shutdown-1"></a>

### get_shutdown/1 ###

<pre><code>
get_shutdown(Agent_spec::<a href="#type-spec">spec()</a>) -&gt; <a href="#type-shutdown">shutdown()</a> | undefined
</code></pre>
<br />

<a name="get_start-1"></a>

### get_start/1 ###

<pre><code>
get_start(Agent_spec::<a href="#type-spec">spec()</a>) -&gt; <a href="#type-mfargs">mfargs()</a> | undefined
</code></pre>
<br />

<a name="is_spec-1"></a>

### is_spec/1 ###

<pre><code>
is_spec(X::<a href="#type-spec">spec()</a> | term()) -&gt; boolean()
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Start::<a href="#type-mfargs">mfargs()</a>, Restart::<a href="logi_restart_strategy.md#type-strategy">logi_restart_strategy:strategy()</a>, Shutdown::<a href="#type-shutdown">shutdown()</a>) -&gt; <a href="#type-spec">spec()</a>
</code></pre>
<br />

<a name="new_external-3"></a>

### new_external/3 ###

<pre><code>
new_external(AgentRef::<a href="#type-proc_ref">proc_ref()</a>, Restart::<a href="logi_restart_strategy.md#type-strategy">logi_restart_strategy:strategy()</a>, ExtraData::<a href="logi_sink.md#type-extra_data">logi_sink:extra_data()</a>) -&gt; <a href="#type-spec">spec()</a>
</code></pre>
<br />

<a name="new_opaque-1"></a>

### new_opaque/1 ###

<pre><code>
new_opaque(ExtraData::<a href="logi_sink.md#type-extra_data">logi_sink:extra_data()</a>) -&gt; <a href="#type-spec">spec()</a>
</code></pre>
<br />

<a name="start_agent-2"></a>

### start_agent/2 ###

<pre><code>
start_agent(Opaque_agent::<a href="#type-spec">spec()</a>, AgentSup::pid()) -&gt; {ok, pid(), <a href="logi_sink.md#type-extra_data">logi_sink:extra_data()</a>} | {error, Reason::term()} | {ignore, <a href="logi_sink.md#type-extra_data">logi_sink:extra_data()</a>}
</code></pre>
<br />

<a name="whereis_name-1"></a>

### whereis_name/1 ###

<pre><code>
whereis_name(X::<a href="#type-proc_ref">proc_ref()</a>) -&gt; pid() | undefined
</code></pre>
<br />

