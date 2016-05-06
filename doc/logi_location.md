

# Module logi_location #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The location where log message issued.

Copyright (c) 2014-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > Location = logi_location:new(lists, filter, 10).
  > logi_location:to_map(Location).
  #{application => stdlib,
    function => filter,
    line => 10,
    module => lists,
    process => <0.91.0>}
```

[`guess_location/0`](#guess_location-0) returns the current location.

```erlang

  > Location = logi_location:guess_location(). % If <code>logi_transform</code> is not used, a warning will be emitted.
  =WARNING REPORT==== 19-Oct-2015::14:02:26 ===
     pid: <0.91.0>
     module: erl_eval
     function: do_apply
     line: 673
     msg: "A deprecated function 'logi_location:guess_location/0' is called. Please use the <code>{parse_transform, logi_transform}</code> compiler option."
  > logi_location:to_map(Location).
  #{application => stdlib,
    function => do_apply,
    line => 673,
    module => erl_eval,
    process => <0.91.0>}
```

<a name="types"></a>

## Data Types ##




### <a name="type-application">application()</a> ###


<pre><code>
application() = atom()
</code></pre>

 An application name



### <a name="type-line">line()</a> ###


<pre><code>
line() = pos_integer() | 0
</code></pre>

 A line number

`0` means "Unknown Line"



### <a name="type-location">location()</a> ###


__abstract datatype__: `location()`

 A log message issued location



### <a name="type-map_form">map_form()</a> ###


<pre><code>
map_form() = #{process =&gt; pid(), application =&gt; <a href="#type-application">application()</a>, module =&gt; module(), function =&gt; atom(), line =&gt; <a href="#type-line">line()</a>}
</code></pre>

 The map representation of a location

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Creates a new location from <code>Map</code></td></tr><tr><td valign="top"><a href="#get_application-1">get_application/1</a></td><td>Gets the application of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_function-1">get_function/1</a></td><td>Gets the function of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_line-1">get_line/1</a></td><td>Gets the line of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Gets the module of <code>Location</code></td></tr><tr><td valign="top"><a href="#get_process-1">get_process/1</a></td><td>Gets the PID of <code>Location</code></td></tr><tr><td valign="top"><a href="#guess_application-1">guess_application/1</a></td><td>Guesses the application to which <code>Module</code> belongs.</td></tr><tr><td valign="top"><a href="#guess_location-0">guess_location/0</a></td><td>(<em>Deprecated</em>.) Guesses the location where the function is called (parse transformation fallback).</td></tr><tr><td valign="top"><a href="#is_location-1">is_location/1</a></td><td>Returns <code>true</code> if <code>X</code> is a location object, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Equivalent to <a href="#new-5"><tt>new(self(), guess_application(Module), Module, Function,
Line)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-5">new/5</a></td><td>Creates a new location object.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>Converts <code>Location</code> into a map form.</td></tr><tr><td valign="top"><a href="#unsafe_new-5">unsafe_new/5</a></td><td>Equivalent to <a href="#new-5"><code>new/5</code></a> except omission of the arguments validation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-map_form">map_form()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

Creates a new location from `Map`

Default Value:
- process: `self()`
- application: `guess_application(maps:get(module, Map))`
- module: `undefined`
- function: `undefined`
- line: `0`

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

<a name="get_process-1"></a>

### get_process/1 ###

<pre><code>
get_process(Location::<a href="#type-location">location()</a>) -&gt; pid()
</code></pre>
<br />

Gets the PID of `Location`

<a name="guess_application-1"></a>

### guess_application/1 ###

<pre><code>
guess_application(Module::module()) -&gt; atom() | undefined
</code></pre>
<br />

Guesses the application to which `Module` belongs

<a name="guess_location-0"></a>

### guess_location/0 ###

<pre><code>
guess_location() -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

__This function is deprecated:__ Please use the `{parse_transform, logi_transform}` compiler option
which replaces the function call to a more efficient code.

Guesses the location where the function is called (parse transformation fallback)

This function is too slow and provided for debugging/testing purposes only.

<a name="is_location-1"></a>

### is_location/1 ###

<pre><code>
is_location(X::<a href="#type-location">location()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a location object, `false` otherwise.

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Module::module(), Function::atom(), Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

Equivalent to [`new(self(), guess_application(Module), Module, Function,Line)`](#new-5).

<a name="new-5"></a>

### new/5 ###

<pre><code>
new(Pid::pid(), Application::<a href="#type-application">application()</a>, Module::module(), Function::atom(), Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

Creates a new location object

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Location::<a href="#type-location">location()</a>) -&gt; <a href="#type-map_form">map_form()</a>
</code></pre>
<br />

Converts `Location` into a map form

<a name="unsafe_new-5"></a>

### unsafe_new/5 ###

<pre><code>
unsafe_new(Pid::pid(), Application::<a href="#type-application">application()</a>, Module::module(), Function::atom(), Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-location">location()</a>
</code></pre>
<br />

Equivalent to [`new/5`](#new-5) except omission of the arguments validation

