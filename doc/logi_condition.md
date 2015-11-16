

# Module logi_condition #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2014-2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-condition">condition()</a> ###


<pre><code>
condition() = <a href="#type-severity_condition">severity_condition()</a> | <a href="#type-location_condition">location_condition()</a>
</code></pre>

 The condition to determine which messages to be consumed by a sink.



### <a name="type-location_condition">location_condition()</a> ###


<pre><code>
location_condition() = #{severity =&gt; <a href="#type-severity_condition">severity_condition()</a>, application =&gt; <a href="logi_location.md#type-application">logi_location:application()</a> | [<a href="logi_location.md#type-application">logi_location:application()</a>], module =&gt; module() | [module()]}
</code></pre>

 The messages which satisfy `severity` (default is `debug`) and are sent from the specified location will be consumed.

The location is specified by `application` and `module` (OR condition).

NOTE: The modules which does not belong to any application are forbidden.


#### <a name="EXAMPLE">EXAMPLE</a> ####


```erlang

  > logi_sink:is_condition(#{application => stdlib}).                          % application
  > logi_sink:is_condition(#{application => [stdlib, kernel]}).                % applications
  > logi_sink:is_condition(#{module => lists}).                                % module
  > logi_sink:is_condition(#{module => [lists, dict]}).                        % modules
  > logi_sink:is_condition(#{application => kernel, module => [lists, dict]}). % application and modules
  > logi_sink:is_condition(#{severity => [info, alert], module => lists}).     % severity and module
```



### <a name="type-normalized_condition">normalized_condition()</a> ###


<pre><code>
normalized_condition() = [<a href="logi.md#type-severity">logi:severity()</a> | {<a href="logi.md#type-severity">logi:severity()</a>, <a href="logi_location.md#type-application">logi_location:application()</a>} | {<a href="logi.md#type-severity">logi:severity()</a>, <a href="logi_location.md#type-application">logi_location:application()</a>, module()}]
</code></pre>

 The normalized form of a `condition/0`.

```erlang

  > Normalize = fun (C) -> lists:sort(logi_sink:normalize_condition(C)) end.
  > Normalize(info).
  [alert,critical,emergency,error,info,notice,warning]
  > Normalize({info, alert}).
  [alert,critical,error,info,notice,warning]
  > Normalize(#{severity => [info], application => [kernel, stdlib]}).
  [{info,kernel},{info,stdlib}]
  > Normalize(#{severity => [info], module => [lists, logi]}).
  [{info,logi,logi},{info,stdlib,lists}]
  > Normalize(#{severity => [info], application => kernel, module => [lists, logi]}).
  [{info,kernel},{info,logi,logi},{info,stdlib,lists}]
```



### <a name="type-severity_condition">severity_condition()</a> ###


<pre><code>
severity_condition() = (Min::<a href="logi.md#type-severity">logi:severity()</a>) | {Min::<a href="logi.md#type-severity">logi:severity()</a>, Max::<a href="logi.md#type-severity">logi:severity()</a>} | (Severities::[<a href="logi.md#type-severity">logi:severity()</a>])
</code></pre>

`Min`:
- The messages with `Min` or higher severity will be consumed.

`{Min, Max}`:
- The messages with severity between `Min` and `Max` will be consumed.

`Severities`:
- The messages with severity included in `Severities` will be consumed.


#### <a name="EXAMPLE">EXAMPLE</a> ####


```erlang

  > [emergency,alert]     = logi_sink:normalize_condition(alert).               % level
  > [warning,notice,info] = logi_sink:normalize_condition({info, warning}).      % range
  > [alert,debug,info]    = logi_sink:normalize_condition([debug, info, alert]). % list
```

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_condition-1">is_condition/1</a></td><td>Returns <code>true</code> if <code>X</code> is a valid <code>condition()</code> value, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#normalize-1">normalize/1</a></td><td>Returns a normalized form of <code>Condition</code></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_condition-1"></a>

### is_condition/1 ###

<pre><code>
is_condition(X::<a href="#type-condition">condition()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a valid `condition()` value, otherwise `false`

<a name="normalize-1"></a>

### normalize/1 ###

<pre><code>
normalize(Condition::<a href="#type-condition">condition()</a>) -&gt; <a href="#type-normalized_condition">normalized_condition()</a>
</code></pre>
<br />

Returns a normalized form of `Condition`

