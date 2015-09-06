

# Module logi_builtin_filter_rate_control #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A built-in log message output rate control filter.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="types"></a>

## Data Types ##




### <a name="type-non_neg_seconds">non_neg_seconds()</a> ###


<pre><code>
non_neg_seconds() = non_neg_integer()
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {max_rate, <a href="#type-rate_spec">rate_spec()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-rate_spec">rate_spec()</a> ###


<pre><code>
rate_spec() = #{id =&gt; <a href="#type-rate_spec_id">rate_spec_id()</a>, intensity =&gt; non_neg_integer(), period =&gt; <a href="#type-non_neg_seconds">non_neg_seconds()</a>}
</code></pre>




### <a name="type-rate_spec_id">rate_spec_id()</a> ###


<pre><code>
rate_spec_id() = term()
</code></pre>

XXX: name

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Creates a filter.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>
<br />

Creates a filter

