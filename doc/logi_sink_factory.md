

# Module logi_sink_factory #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `logi_sink_factory` behaviour.__<br /> Required callback functions: `instantiate/1`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = atom()
</code></pre>




### <a name="type-factory">factory()</a> ###


__abstract datatype__: `factory()`




### <a name="type-instantiate_result">instantiate_result()</a> ###


<pre><code>
instantiate_result() = {ok, <a href="logi_sink.md#type-instance">logi_sink:instance()</a>} | {start_child, <a href="#type-start_child_arg">start_child_arg()</a>} | {error, term()}
</code></pre>




### <a name="type-start_child_arg">start_child_arg()</a> ###


<pre><code>
start_child_arg() = <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a> | [term()]
</code></pre>




### <a name="type-start_child_resutl">start_child_resutl()</a> ###


<pre><code>
start_child_resutl() = {ok, pid(), <a href="logi_sink.md#type-instance">logi_sink:instance()</a>} | {error, Reason::term()}
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#instantiate-2">instantiate/2</a></td><td></td></tr><tr><td valign="top"><a href="#instantiate_if_needed-2">instantiate_if_needed/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_callback_module-1">is_callback_module/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_factory-1">is_factory/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Logi_sink_factory::<a href="#type-factory">factory()</a>) -&gt; <a href="#type-callback_module">callback_module()</a>
</code></pre>
<br />

<a name="get_state-1"></a>

### get_state/1 ###

<pre><code>
get_state(Logi_sink_factory::<a href="#type-factory">factory()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

<a name="instantiate-2"></a>

### instantiate/2 ###

<pre><code>
instantiate(Factory::<a href="#type-factory">factory()</a>, Supervisor) -&gt; {ok, <a href="logi_sink.md#type-instance">logi_sink:instance()</a>, ChildPid, ChildId} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Supervisor = pid() | atom() | {global, term()} | {via, module(), term()}</code></li><li><code>ChildPid = pid()</code></li><li><code>ChildId = term()</code></li><li><code>Reason = term()</code></li></ul>

<a name="instantiate_if_needed-2"></a>

### instantiate_if_needed/2 ###

<pre><code>
instantiate_if_needed(Factory::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Supervisor) -&gt; {ok, <a href="logi_sink.md#type-instance">logi_sink:instance()</a>, ChildPid, ChildId} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Supervisor = pid() | atom() | {global, term()} | {via, module(), term()}</code></li><li><code>ChildPid = undefined | pid()</code></li><li><code>ChildId = undefined | term()</code></li><li><code>Reason = term()</code></li></ul>

<a name="is_callback_module-1"></a>

### is_callback_module/1 ###

<pre><code>
is_callback_module(X::<a href="#type-callback_module">callback_module()</a> | term()) -&gt; boolean()
</code></pre>
<br />

<a name="is_factory-1"></a>

### is_factory/1 ###

<pre><code>
is_factory(X::<a href="#type-factory">factory()</a> | term()) -&gt; boolean()
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, State::<a href="#type-state">state()</a>) -&gt; <a href="#type-factory">factory()</a>
</code></pre>
<br />

