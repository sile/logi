

# Module logi_layout #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_layout` behaviour.__<br /> Required callback functions: `format/4`.

<a name="types"></a>

## Data Types ##




### <a name="type-extra_arg">extra_arg()</a> ###


<pre><code>
extra_arg() = term()
</code></pre>




### <a name="type-layout">layout()</a> ###


<pre><code>
layout() = module() | {module(), <a href="#type-extra_arg">extra_arg()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-4">format/4</a></td><td></td></tr><tr><td valign="top"><a href="#is_layout-1">is_layout/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-4"></a>

### format/4 ###

<pre><code>
format(Context::<a href="logi_context.md#type-context">logi_context:context()</a>, Format::<a href="io.md#type-format">io:format()</a>, FormatArgs::[term()], Module::<a href="#type-layout">layout()</a>) -&gt; iodata()
</code></pre>
<br />

<a name="is_layout-1"></a>

### is_layout/1 ###

<pre><code>
is_layout(Module::<a href="#type-layout">layout()</a> | term()) -&gt; boolean()
</code></pre>
<br />

