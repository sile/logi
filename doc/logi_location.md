

# Module logi_location #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Message issued location.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-line">line()</a> ###


<pre><code>
line() = pos_integer() | 0
</code></pre>

0 indicates 'Unknown Line'



### <a name="type-location">location()</a> ###


__abstract datatype__: `location()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_application-1">get_application/1</a></td><td>Gets the application of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_function-1">get_function/1</a></td><td>Gets the function of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_line-1">get_line/1</a></td><td>Gets the line of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_node-1">get_node/1</a></td><td>Gets the node name of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_process-1">get_process/1</a></td><td>Gets the PID of <code>Location</code></td></tr><tr><td valign="top"><a href="#guess-0">guess/0</a></td><td></td></tr><tr><td valign="top"><a href="#guess_application-1">guess_application/1</a></td><td>Guesses the application to which <code>Module</code> belongs.</td></tr><tr><td valign="top"><a href="#is_location-1">is_location/1</a></td><td>TODO.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Equivalent to <a href="#new-6"><tt>new(node(), self(), guess_application(Module), Module,
Function, Line)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td>Creates a new location object.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>Converts <code>Location</code> to a map.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_application-1"></a>

### get_application/1 ###

<pre><code>
get_application(Location::<a href="#type-location">location()</a>) -&gt; atom()
</code></pre>
<br />

Gets the application of `Location`

<a name="get_function-1"></a>

### get_function/1 ###

<pre><code>
get_function(Location::<a href="#type-location">location()</a>) -&gt; atom()
</code></pre>
<br />

Gets the function of `Location`

<a name="get_line-1"></a>

### get_line/1 ###

<pre><code>
get_line(Location::<a href="#type-location">location()</a>) -&gt; <a href="#type-line">line()</a>
</code></pre>
<br />

Gets the line of `Location`

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Location::<a href="#type-location">location()</a>) -&gt; module()
</code></pre>
<br />

Gets the module of `Location`

<a name="get_node-1"></a>

### get_node/1 ###

<pre><code>
get_node(Location::<a href="#type-location">location()</a>) -&gt; node()
</code></pre>
<br />

Gets the node name of `Location`

<a name="get_process-1"></a>

### get_process/1 ###

<pre><code>
get_process(Location::<a href="#type-location">location()</a>) -&gt; pid()
</code></pre>
<br />

Gets the PID of `Location`

<a name="guess-0"></a>

### guess/0 ###

<pre><code>
guess() -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

<a name="guess_application-1"></a>

### guess_application/1 ###

<pre><code>
guess_application(Module::module()) -&gt; atom() | undefined
</code></pre>
<br />

Guesses the application to which `Module` belongs

<a name="is_location-1"></a>

### is_location/1 ###

<pre><code>
is_location(X::<a href="#type-location">location()</a> | term()) -&gt; boolean()
</code></pre>
<br />

TODO

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Module::module(), Function::atom(), Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

Equivalent to [`new(node(), self(), guess_application(Module), Module,Function, Line)`](#new-6).

<a name="new-6"></a>

### new/6 ###

<pre><code>
new(Node::node(), Pid::pid(), Application::atom(), Module::module(), Function::atom(), Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

Creates a new location object

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Location::<a href="#type-location">location()</a>) -&gt; Map
</code></pre>

<ul class="definitions"><li><code>Map = #{node =&gt; node(), process =&gt; pid(), application =&gt; atom(), module =&gt; module(), function =&gt; atom(), line =&gt; <a href="#type-line">line()</a>}</code></li></ul>

Converts `Location` to a map

