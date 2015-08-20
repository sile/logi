

# Module logi #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

ログ出力用の各種機能を提供するモジュール.

Copyright (c) 2014 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-context">context()</a> ###


<pre><code>
context() = <a href="logi_context.md#type-context">logi_context:context()</a>
</code></pre>

opaqueにしたい



### <a name="type-context_id">context_id()</a> ###


<pre><code>
context_id() = atom()
</code></pre>




### <a name="type-context_ref">context_ref()</a> ###


<pre><code>
context_ref() = <a href="#type-context">context()</a> | <a href="#type-context_id">context_id()</a>
</code></pre>




### <a name="type-frequency_policy">frequency_policy()</a> ###


<pre><code>
frequency_policy() = always | once | {once_in_times, Times::pos_integer()} | {interval, MilliSeconds::non_neg_integer()}
</code></pre>




### <a name="type-header">header()</a> ###


<pre><code>
header() = {<a href="#type-header_key">header_key()</a>, <a href="#type-header_value">header_value()</a>}
</code></pre>




### <a name="type-header_key">header_key()</a> ###


<pre><code>
header_key() = term()
</code></pre>




### <a name="type-header_value">header_value()</a> ###


<pre><code>
header_value() = term()
</code></pre>




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = [<a href="#type-header">header()</a>]
</code></pre>




### <a name="type-log_level">log_level()</a> ###


<pre><code>
log_level() = debug | verbose | info | notice | warning | error | critical | alert | emergency
</code></pre>




### <a name="type-log_option">log_option()</a> ###


<pre><code>
log_option() = {headers, <a href="#type-headers">headers()</a>} | {metadata, <a href="#type-metadata">metadata()</a>} | {frequency, <a href="#type-frequency_policy">frequency_policy()</a>}
</code></pre>

default: always



### <a name="type-log_options">log_options()</a> ###


<pre><code>
log_options() = [<a href="#type-log_option">log_option()</a>]
</code></pre>




### <a name="type-logger">logger()</a> ###


<pre><code>
logger() = atom()
</code></pre>




### <a name="type-metadata">metadata()</a> ###


<pre><code>
metadata() = [<a href="#type-metadata_entry">metadata_entry()</a>]
</code></pre>




### <a name="type-metadata_entry">metadata_entry()</a> ###


<pre><code>
metadata_entry() = {<a href="#type-metadata_entry_key">metadata_entry_key()</a>, <a href="#type-metadata_entry_value">metadata_entry_value()</a>}
</code></pre>




### <a name="type-metadata_entry_key">metadata_entry_key()</a> ###


<pre><code>
metadata_entry_key() = term()
</code></pre>




### <a name="type-metadata_entry_value">metadata_entry_value()</a> ###


<pre><code>
metadata_entry_value() = term()
</code></pre>




### <a name="type-severity">severity()</a> ###


<pre><code>
severity() = <a href="#type-log_level">log_level()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alert-1">alert/1</a></td><td></td></tr><tr><td valign="top"><a href="#alert-2">alert/2</a></td><td></td></tr><tr><td valign="top"><a href="#alert-3">alert/3</a></td><td></td></tr><tr><td valign="top"><a href="#alert_opt-2">alert_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#alert_opt-3">alert_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#alert_opt-4">alert_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#clear_headers-0">clear_headers/0</a></td><td>Equivalent to <a href="#clear_headers-1"><tt>clear_headers(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#clear_headers-1">clear_headers/1</a></td><td>ヘッダ群をクリアする.</td></tr><tr><td valign="top"><a href="#clear_metadata-0">clear_metadata/0</a></td><td>Equivalent to <a href="#clear_metadata-1"><tt>clear_metadata(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#clear_metadata-1">clear_metadata/1</a></td><td>メタデータをクリアする.</td></tr><tr><td valign="top"><a href="#critical-1">critical/1</a></td><td></td></tr><tr><td valign="top"><a href="#critical-2">critical/2</a></td><td></td></tr><tr><td valign="top"><a href="#critical-3">critical/3</a></td><td></td></tr><tr><td valign="top"><a href="#critical_opt-2">critical_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#critical_opt-3">critical_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#critical_opt-4">critical_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#debug-1">debug/1</a></td><td></td></tr><tr><td valign="top"><a href="#debug-2">debug/2</a></td><td></td></tr><tr><td valign="top"><a href="#debug-3">debug/3</a></td><td></td></tr><tr><td valign="top"><a href="#debug_opt-2">debug_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#debug_opt-3">debug_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#debug_opt-4">debug_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#default_logger-0">default_logger/0</a></td><td>デフォルトのロガーを返す.</td></tr><tr><td valign="top"><a href="#delete_backend-1">delete_backend/1</a></td><td>Equivalent to <a href="#delete_backend-2"><tt>delete_backend(default_logger(), BackendId)</tt></a>.</td></tr><tr><td valign="top"><a href="#delete_backend-2">delete_backend/2</a></td><td>バックエンドを削除する.</td></tr><tr><td valign="top"><a href="#delete_headers-1">delete_headers/1</a></td><td>Equivalent to <a href="#default_logger-2"><tt>default_logger(default_logger(), Keys)</tt></a>.</td></tr><tr><td valign="top"><a href="#delete_headers-2">delete_headers/2</a></td><td>指定されたキーを保持するヘッダ群の削除する.</td></tr><tr><td valign="top"><a href="#delete_metadata-1">delete_metadata/1</a></td><td>Equivalent to <a href="#delete_metadata-2"><tt>delete_metadata(default_logger(), Keys)</tt></a>.</td></tr><tr><td valign="top"><a href="#delete_metadata-2">delete_metadata/2</a></td><td>メタデータから指定されたキーのエントリ群の削除する.</td></tr><tr><td valign="top"><a href="#emergency-1">emergency/1</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-2">emergency/2</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-3">emergency/3</a></td><td></td></tr><tr><td valign="top"><a href="#emergency_opt-2">emergency_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#emergency_opt-3">emergency_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#emergency_opt-4">emergency_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_logger_started-1">ensure_logger_started/1</a></td><td>まだ未起動の場合は、指定のロガーを起動する.</td></tr><tr><td valign="top"><a href="#error-1">error/1</a></td><td></td></tr><tr><td valign="top"><a href="#error-2">error/2</a></td><td></td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td></td></tr><tr><td valign="top"><a href="#error_opt-2">error_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#error_opt-3">error_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#error_opt-4">error_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#find_backend-1">find_backend/1</a></td><td>Equivalent to <a href="#find_backend-2"><tt>find_backend(default_logger(), BackendId)</tt></a>.</td></tr><tr><td valign="top"><a href="#find_backend-2">find_backend/2</a></td><td>バックエンドを検索する.</td></tr><tr><td valign="top"><a href="#get_condition-1">get_condition/1</a></td><td>Equivalent to <a href="#get_condition-2"><tt>get_condition(default_logger(), BackendId)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_condition-2">get_condition/2</a></td><td>バックエンドのログ出力条件を取得する.</td></tr><tr><td valign="top"><a href="#get_headers-0">get_headers/0</a></td><td>Equivalent to <a href="#get_headers-1"><tt>get_headers(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#get_headers-1">get_headers/1</a></td><td>ヘッダ群を取得する.</td></tr><tr><td valign="top"><a href="#get_metadata-0">get_metadata/0</a></td><td>Equivalent to <a href="#get_metadata-1"><tt>get_metadata(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#get_metadata-1">get_metadata/1</a></td><td>メタデータを取得する.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td></td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td></td></tr><tr><td valign="top"><a href="#info_opt-2">info_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#info_opt-3">info_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#info_opt-4">info_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#load_context-0">load_context/0</a></td><td>Equivalent to <a href="#load_context-1"><tt>load_context(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#load_context-1">load_context/1</a></td><td>ログ出力コンテキストをプロセス辞書からロードする.</td></tr><tr><td valign="top"><a href="#log-5">log/5</a></td><td>Equivalent to <a href="#log-6"><tt>log(default_logger(), Severity, Location, Format, Args,
Options)</tt></a>.</td></tr><tr><td valign="top"><a href="#log-6">log/6</a></td><td>ログを出力する.</td></tr><tr><td valign="top"><a href="#log_levels-0">log_levels/0</a></td><td>利用可能なログレベル一覧を返す.</td></tr><tr><td valign="top"><a href="#make_context-0">make_context/0</a></td><td>Equivalent to <a href="#make_context-1"><tt>make_context(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#make_context-1">make_context/1</a></td><td>Equivalent to <a href="#make_context-2"><tt>make_context(LoggerId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#make_context-2">make_context/2</a></td><td>ログ出力コンテキストを生成する.</td></tr><tr><td valign="top"><a href="#notice-1">notice/1</a></td><td></td></tr><tr><td valign="top"><a href="#notice-2">notice/2</a></td><td></td></tr><tr><td valign="top"><a href="#notice-3">notice/3</a></td><td></td></tr><tr><td valign="top"><a href="#notice_opt-2">notice_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#notice_opt-3">notice_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#notice_opt-4">notice_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#save_context-1">save_context/1</a></td><td>Equivalent to <a href="#save_context-2"><tt>save_context(default_logger(), Context)</tt></a>.</td></tr><tr><td valign="top"><a href="#save_context-2">save_context/2</a></td><td>ログ出力コンテキストをプロセス辞書に保存する.</td></tr><tr><td valign="top"><a href="#set_backend-2">set_backend/2</a></td><td>Equivalent to <a href="#set_backend-3"><tt>set_backend(default_logger(), BackendSpec,
ConditionSpec)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_backend-3">set_backend/3</a></td><td>バックエンドを登録する.</td></tr><tr><td valign="top"><a href="#set_condition-2">set_condition/2</a></td><td>Equivalent to <a href="#set_condition-3"><tt>set_condition(default_logger(), BackendId,
ConditionSpec)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_condition-3">set_condition/3</a></td><td>バックエンドのログ出力条件を設定する.</td></tr><tr><td valign="top"><a href="#set_headers-1">set_headers/1</a></td><td>Equivalent to <a href="#set_headers-2"><tt>set_headers(default_logger(), Headers)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_headers-2">set_headers/2</a></td><td>ヘッダ群を設定する.</td></tr><tr><td valign="top"><a href="#set_metadata-1">set_metadata/1</a></td><td>Equivalent to <a href="#set_metadata-2"><tt>set_metadata(default_logger(), MetaData)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_metadata-2">set_metadata/2</a></td><td>ヘッダ群を設定する.</td></tr><tr><td valign="top"><a href="#start_logger-1">start_logger/1</a></td><td>ロガーを起動する.</td></tr><tr><td valign="top"><a href="#stop_logger-1">stop_logger/1</a></td><td>ロガーを停止する.</td></tr><tr><td valign="top"><a href="#verbose-1">verbose/1</a></td><td></td></tr><tr><td valign="top"><a href="#verbose-2">verbose/2</a></td><td></td></tr><tr><td valign="top"><a href="#verbose-3">verbose/3</a></td><td></td></tr><tr><td valign="top"><a href="#verbose_opt-2">verbose_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#verbose_opt-3">verbose_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#verbose_opt-4">verbose_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#warning-1">warning/1</a></td><td></td></tr><tr><td valign="top"><a href="#warning-2">warning/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning-3">warning/3</a></td><td></td></tr><tr><td valign="top"><a href="#warning_opt-2">warning_opt/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning_opt-3">warning_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#warning_opt-4">warning_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#which_backends-0">which_backends/0</a></td><td>Equivalent to <a href="#which_backends-1"><tt>which_backends(default_logger())</tt></a>.</td></tr><tr><td valign="top"><a href="#which_backends-1">which_backends/1</a></td><td>登録されているバックエンド一覧を取得する.</td></tr><tr><td valign="top"><a href="#which_contexts-0">which_contexts/0</a></td><td>保存されているコンテキスト一覧を取得する.</td></tr><tr><td valign="top"><a href="#which_loggers-0">which_loggers/0</a></td><td>起動中のロガー一覧を取得する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alert-1"></a>

### alert/1 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="alert-2"></a>

### alert/2 ###

<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="alert-3"></a>

### alert/3 ###

<pre><code>
alert(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="alert_opt-2"></a>

### alert_opt/2 ###

<pre><code>
alert_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="alert_opt-3"></a>

### alert_opt/3 ###

<pre><code>
alert_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="alert_opt-4"></a>

### alert_opt/4 ###

<pre><code>
alert_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="clear_headers-0"></a>

### clear_headers/0 ###

`clear_headers() -> any()`

Equivalent to [`clear_headers(default_logger())`](#clear_headers-1).

<a name="clear_headers-1"></a>

### clear_headers/1 ###

<pre><code>
clear_headers(ContextRef::<a href="#type-context_ref">context_ref()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

ヘッダ群をクリアする

<a name="clear_metadata-0"></a>

### clear_metadata/0 ###

`clear_metadata() -> any()`

Equivalent to [`clear_metadata(default_logger())`](#clear_metadata-1).

<a name="clear_metadata-1"></a>

### clear_metadata/1 ###

<pre><code>
clear_metadata(ContextRef::<a href="#type-context_ref">context_ref()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

メタデータをクリアする

<a name="critical-1"></a>

### critical/1 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="critical-2"></a>

### critical/2 ###

<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="critical-3"></a>

### critical/3 ###

<pre><code>
critical(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="critical_opt-2"></a>

### critical_opt/2 ###

<pre><code>
critical_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="critical_opt-3"></a>

### critical_opt/3 ###

<pre><code>
critical_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="critical_opt-4"></a>

### critical_opt/4 ###

<pre><code>
critical_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="debug-1"></a>

### debug/1 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="debug-2"></a>

### debug/2 ###

<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="debug-3"></a>

### debug/3 ###

<pre><code>
debug(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="debug_opt-2"></a>

### debug_opt/2 ###

<pre><code>
debug_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="debug_opt-3"></a>

### debug_opt/3 ###

<pre><code>
debug_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="debug_opt-4"></a>

### debug_opt/4 ###

<pre><code>
debug_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="default_logger-0"></a>

### default_logger/0 ###

<pre><code>
default_logger() -&gt; <a href="#type-logger">logger()</a>
</code></pre>
<br />

デフォルトのロガーを返す

このロガーはlogiアプリケーションの開始に合わせて自動的に起動される

<a name="delete_backend-1"></a>

### delete_backend/1 ###

<pre><code>
delete_backend(BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to [`delete_backend(default_logger(), BackendId)`](#delete_backend-2).

<a name="delete_backend-2"></a>

### delete_backend/2 ###

<pre><code>
delete_backend(LoggerId::<a href="#type-logger">logger()</a>, BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>) -&gt; ok
</code></pre>
<br />

バックエンドを削除する

存在しないバックエンドが指定された場合でもエラーとはならずに単に無視される

<a name="delete_headers-1"></a>

### delete_headers/1 ###

<pre><code>
delete_headers(Keys::[<a href="#type-header_key">header_key()</a>]) -&gt; <a href="#type-context_id">context_id()</a>
</code></pre>
<br />

Equivalent to [`default_logger(default_logger(), Keys)`](#default_logger-2).

<a name="delete_headers-2"></a>

### delete_headers/2 ###

<pre><code>
delete_headers(ContextRef::<a href="#type-context_ref">context_ref()</a>, Keys::[<a href="#type-header_key">header_key()</a>]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

指定されたキーを保持するヘッダ群の削除する

<a name="delete_metadata-1"></a>

### delete_metadata/1 ###

<pre><code>
delete_metadata(Keys::[<a href="#type-header_key">header_key()</a>]) -&gt; <a href="#type-context_id">context_id()</a>
</code></pre>
<br />

Equivalent to [`delete_metadata(default_logger(), Keys)`](#delete_metadata-2).

<a name="delete_metadata-2"></a>

### delete_metadata/2 ###

<pre><code>
delete_metadata(ContextRef::<a href="#type-context_ref">context_ref()</a>, Keys::[<a href="#type-header_key">header_key()</a>]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

メタデータから指定されたキーのエントリ群の削除する

<a name="emergency-1"></a>

### emergency/1 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="emergency-2"></a>

### emergency/2 ###

<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="emergency-3"></a>

### emergency/3 ###

<pre><code>
emergency(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="emergency_opt-2"></a>

### emergency_opt/2 ###

<pre><code>
emergency_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="emergency_opt-3"></a>

### emergency_opt/3 ###

<pre><code>
emergency_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="emergency_opt-4"></a>

### emergency_opt/4 ###

<pre><code>
emergency_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="ensure_logger_started-1"></a>

### ensure_logger_started/1 ###

<pre><code>
ensure_logger_started(LoggerId::<a href="#type-logger">logger()</a>) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

まだ未起動の場合は、指定のロガーを起動する

<a name="error-1"></a>

### error/1 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="error-2"></a>

### error/2 ###

<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="error-3"></a>

### error/3 ###

<pre><code>
error(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="error_opt-2"></a>

### error_opt/2 ###

<pre><code>
error_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="error_opt-3"></a>

### error_opt/3 ###

<pre><code>
error_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="error_opt-4"></a>

### error_opt/4 ###

<pre><code>
error_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="find_backend-1"></a>

### find_backend/1 ###

<pre><code>
find_backend(BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>) -&gt; {ok, <a href="logi_backend.md#type-backend">logi_backend:backend()</a>} | {error, not_found}
</code></pre>
<br />

Equivalent to [`find_backend(default_logger(), BackendId)`](#find_backend-2).

<a name="find_backend-2"></a>

### find_backend/2 ###

<pre><code>
find_backend(LoggerId::<a href="#type-logger">logger()</a>, BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>) -&gt; {ok, <a href="logi_backend.md#type-backend">logi_backend:backend()</a>} | {error, not_found}
</code></pre>
<br />

バックエンドを検索する

<a name="get_condition-1"></a>

### get_condition/1 ###

<pre><code>
get_condition(BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>) -&gt; {ok, <a href="logi_condition.md#type-condition">logi_condition:condition()</a>} | {error, not_found}
</code></pre>
<br />

Equivalent to [`get_condition(default_logger(), BackendId)`](#get_condition-2).

<a name="get_condition-2"></a>

### get_condition/2 ###

<pre><code>
get_condition(LoggerId::<a href="#type-logger">logger()</a>, BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>) -&gt; {ok, <a href="logi_condition.md#type-condition">logi_condition:condition()</a>} | {error, not_found}
</code></pre>
<br />

バックエンドのログ出力条件を取得する

<a name="get_headers-0"></a>

### get_headers/0 ###

<pre><code>
get_headers() -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

Equivalent to [`get_headers(default_logger())`](#get_headers-1).

<a name="get_headers-1"></a>

### get_headers/1 ###

<pre><code>
get_headers(ContextRef::<a href="#type-context_ref">context_ref()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

ヘッダ群を取得する

<a name="get_metadata-0"></a>

### get_metadata/0 ###

<pre><code>
get_metadata() -&gt; <a href="#type-metadata">metadata()</a>
</code></pre>
<br />

Equivalent to [`get_metadata(default_logger())`](#get_metadata-1).

<a name="get_metadata-1"></a>

### get_metadata/1 ###

<pre><code>
get_metadata(ContextRef::<a href="#type-context_ref">context_ref()</a>) -&gt; <a href="#type-metadata">metadata()</a>
</code></pre>
<br />

メタデータを取得する

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="info-2"></a>

### info/2 ###

<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="info-3"></a>

### info/3 ###

<pre><code>
info(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="info_opt-2"></a>

### info_opt/2 ###

<pre><code>
info_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="info_opt-3"></a>

### info_opt/3 ###

<pre><code>
info_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="info_opt-4"></a>

### info_opt/4 ###

<pre><code>
info_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="load_context-0"></a>

### load_context/0 ###

<pre><code>
load_context() -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Equivalent to [`load_context(default_logger())`](#load_context-1).

<a name="load_context-1"></a>

### load_context/1 ###

<pre><code>
load_context(ContextId::<a href="#type-context_id">context_id()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

ログ出力コンテキストをプロセス辞書からロードする

指定されたIDのコンテキストが存在しない場合は`make_context(ContextId)`が代わりのデフォルト値として使用される。<br />

<a name="log-5"></a>

### log/5 ###

<pre><code>
log(Severity::<a href="#type-severity">severity()</a>, Location::<a href="logi_location.md#type-location">logi_location:location()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

Equivalent to [`log(default_logger(), Severity, Location, Format, Args,Options)`](#log-6).

<a name="log-6"></a>

### log/6 ###

<pre><code>
log(ContextRef::<a href="#type-context_ref">context_ref()</a>, Severity::<a href="#type-severity">severity()</a>, Location::<a href="logi_location.md#type-location">logi_location:location()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

ログを出力する

通常は`logi_transform`モジュールが適用する関数マクロ群を通してログ出力を行い、この関数は直接は使用されない

<a name="log_levels-0"></a>

### log_levels/0 ###

<pre><code>
log_levels() -&gt; [<a href="#type-log_level">log_level()</a>]
</code></pre>
<br />

利用可能なログレベル一覧を返す

結果の並び順はログレベルの昇順

<a name="make_context-0"></a>

### make_context/0 ###

<pre><code>
make_context() -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Equivalent to [`make_context(default_logger())`](#make_context-1).

<a name="make_context-1"></a>

### make_context/1 ###

<pre><code>
make_context(LoggerId::<a href="#type-logger">logger()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Equivalent to [`make_context(LoggerId, [])`](#make_context-2).

<a name="make_context-2"></a>

### make_context/2 ###

<pre><code>
make_context(LoggerId::<a href="#type-logger">logger()</a>, Options) -&gt; <a href="#type-context">context()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {headers, <a href="#type-headers">headers()</a>} | {metadata, <a href="#type-metadata">metadata()</a>}</code></li></ul>

ログ出力コンテキストを生成する

<a name="notice-1"></a>

### notice/1 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="notice-2"></a>

### notice/2 ###

<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="notice-3"></a>

### notice/3 ###

<pre><code>
notice(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="notice_opt-2"></a>

### notice_opt/2 ###

<pre><code>
notice_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="notice_opt-3"></a>

### notice_opt/3 ###

<pre><code>
notice_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="notice_opt-4"></a>

### notice_opt/4 ###

<pre><code>
notice_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="save_context-1"></a>

### save_context/1 ###

<pre><code>
save_context(Context::<a href="#type-context">context()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to [`save_context(default_logger(), Context)`](#save_context-2).

<a name="save_context-2"></a>

### save_context/2 ###

<pre><code>
save_context(ContextId::<a href="#type-context_id">context_id()</a>, Context::<a href="#type-context">context()</a>) -&gt; ok
</code></pre>
<br />

ログ出力コンテキストをプロセス辞書に保存する

<a name="set_backend-2"></a>

### set_backend/2 ###

<pre><code>
set_backend(BackendSpec::<a href="logi_backend.md#type-spec">logi_backend:spec()</a>, ConditionSpec::<a href="logi_condition.md#type-spec">logi_condition:spec()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to [`set_backend(default_logger(), BackendSpec,ConditionSpec)`](#set_backend-3).

<a name="set_backend-3"></a>

### set_backend/3 ###

<pre><code>
set_backend(LoggerId::<a href="#type-logger">logger()</a>, BackendSpec::<a href="logi_backend.md#type-spec">logi_backend:spec()</a>, ConditionSpec::<a href="logi_condition.md#type-spec">logi_condition:spec()</a>) -&gt; ok
</code></pre>
<br />

バックエンドを登録する

既に同じIDのバックエンドが登録済みの場合は、内容が更新される

<a name="set_condition-2"></a>

### set_condition/2 ###

<pre><code>
set_condition(BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>, ConditionSpec::<a href="logi_condition.md#type-spec">logi_condition:spec()</a>) -&gt; ok | {error, not_found}
</code></pre>
<br />

Equivalent to [`set_condition(default_logger(), BackendId,ConditionSpec)`](#set_condition-3).

<a name="set_condition-3"></a>

### set_condition/3 ###

<pre><code>
set_condition(LoggerId::<a href="#type-logger">logger()</a>, BackendId::<a href="logi_backend.md#type-id">logi_backend:id()</a>, ConditionSpec::<a href="logi_condition.md#type-spec">logi_condition:spec()</a>) -&gt; ok | {error, not_found}
</code></pre>
<br />

バックエンドのログ出力条件を設定する

<a name="set_headers-1"></a>

### set_headers/1 ###

<pre><code>
set_headers(Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-context_id">context_id()</a>
</code></pre>
<br />

Equivalent to [`set_headers(default_logger(), Headers)`](#set_headers-2).

<a name="set_headers-2"></a>

### set_headers/2 ###

<pre><code>
set_headers(ContextRef::<a href="#type-context_ref">context_ref()</a>, Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

ヘッダ群を設定する

キー名が同じヘッダが存在する場合は値が上書きされる. <br />
それ以外の既存のヘッダは、そのまま保持され続ける.

<a name="set_metadata-1"></a>

### set_metadata/1 ###

<pre><code>
set_metadata(MetaData::<a href="#type-metadata">metadata()</a>) -&gt; <a href="#type-context_id">context_id()</a>
</code></pre>
<br />

Equivalent to [`set_metadata(default_logger(), MetaData)`](#set_metadata-2).

<a name="set_metadata-2"></a>

### set_metadata/2 ###

<pre><code>
set_metadata(ContextRef::<a href="#type-context_ref">context_ref()</a>, MetaData::<a href="#type-metadata">metadata()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

ヘッダ群を設定する

キー名が同じエントリが存在する場合は値が上書きされる. <br />
それ以外の既存のエントリは、そのまま保持され続ける.

<a name="start_logger-1"></a>

### start_logger/1 ###

<pre><code>
start_logger(LoggerId::<a href="#type-logger">logger()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = already_started | term()</code></li></ul>

ロガーを起動する

既に同名のロガーが起動している場合は`{error, already_started}`が返される

<a name="stop_logger-1"></a>

### stop_logger/1 ###

<pre><code>
stop_logger(LoggerId::<a href="#type-logger">logger()</a>) -&gt; ok
</code></pre>
<br />

ロガーを停止する

指定のロガーが存在しない場合でもエラーとはならずに単に無視される

<a name="verbose-1"></a>

### verbose/1 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="verbose-2"></a>

### verbose/2 ###

<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="verbose-3"></a>

### verbose/3 ###

<pre><code>
verbose(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="verbose_opt-2"></a>

### verbose_opt/2 ###

<pre><code>
verbose_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="verbose_opt-3"></a>

### verbose_opt/3 ###

<pre><code>
verbose_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="verbose_opt-4"></a>

### verbose_opt/4 ###

<pre><code>
verbose_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="warning-1"></a>

### warning/1 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="warning-2"></a>

### warning/2 ###

<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="warning-3"></a>

### warning/3 ###

<pre><code>
warning(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="warning_opt-2"></a>

### warning_opt/2 ###

<pre><code>
warning_opt(Format::<a href="io.md#type-format">io:format()</a>, Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="warning_opt-3"></a>

### warning_opt/3 ###

<pre><code>
warning_opt(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="warning_opt-4"></a>

### warning_opt/4 ###

<pre><code>
warning_opt(ContextRef::<a href="#type-context_ref">context_ref()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()], Options::<a href="#type-log_options">log_options()</a>) -&gt; <a href="#type-context_ref">context_ref()</a>
</code></pre>
<br />

<a name="which_backends-0"></a>

### which_backends/0 ###

<pre><code>
which_backends() -&gt; [<a href="logi_backend.md#type-backend">logi_backend:backend()</a>]
</code></pre>
<br />

Equivalent to [`which_backends(default_logger())`](#which_backends-1).

<a name="which_backends-1"></a>

### which_backends/1 ###

<pre><code>
which_backends(LoggerId::<a href="#type-logger">logger()</a>) -&gt; [<a href="logi_backend.md#type-backend">logi_backend:backend()</a>]
</code></pre>
<br />

登録されているバックエンド一覧を取得する

<a name="which_contexts-0"></a>

### which_contexts/0 ###

<pre><code>
which_contexts() -&gt; [{<a href="#type-context_id">context_id()</a>, <a href="#type-context">context()</a>}]
</code></pre>
<br />

保存されているコンテキスト一覧を取得する

<a name="which_loggers-0"></a>

### which_loggers/0 ###

<pre><code>
which_loggers() -&gt; [<a href="#type-logger">logger()</a>]
</code></pre>
<br />

起動中のロガー一覧を取得する

