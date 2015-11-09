

# Module logi_builtin_filter_fun #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A built-in stateless filter which filters log messages by an arbitrary user defined function.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="description"></a>

## Description ##


### <a name="NOTE">NOTE</a> ###

This module is mainly supposed to be used for ddebugging/testing purposes.

If you want to set a particular filter to a lot of logger instances,
it is recommended to define a specified filter for efficiency reasons.


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > error_logger:tty(false). % Suppresses annoying warning outputs for brevity
  > Sink = logi_builtin_sink_fun:new(fun (_, _, Format, Data) -> io:format(Format ++ "\n", Data) end).
  > {ok, _} = logi_channel:install_sink(info, Sink).
  > FilterFun = fun (C) -> not maps:get(discard, logi_context:get_metadata(C), false) end.
  > Logger = logi:new([{filter, logi_builtin_filter_fun:new(FilterFun)}]).
  > logi:save_as_default(Logger).
  > logi:info("hello world", [], [{messages, #{discard => false}}]). % passed
  hello world
  > logi:info("hello world", [], [{metadata, #{discard => true}}]). % discarded
  % No output: the log message was discarded by the filter
```

<a name="types"></a>

## Data Types ##




### <a name="type-filter_fun">filter_fun()</a> ###


<pre><code>
filter_fun() = fun((<a href="logi_context.md#type-context">logi_context:context()</a>) -&gt; boolean())
</code></pre>

 A log messages filter function

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a filter which filters log messages by <code>FilterFun</code></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(FilterFun) -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter</a>(FilterFun)
</code></pre>

<ul class="definitions"><li><code>FilterFun = <a href="#type-filter_fun">filter_fun()</a></code></li></ul>

Creates a filter which filters log messages by `FilterFun`

