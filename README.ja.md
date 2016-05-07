logi
====

[![hex.pm version](https://img.shields.io/hexpm/v/logi.svg)](https://hex.pm/packages/logi)

Erlangでログ出力を行うためのインタフェースを提供するライブラリ。

主にサーバライブラリに対して軽量かつ可搬なログ出力方法を提供するのが目的。

特徴
----

- ログ出力のためのインターフェース関数のみを提供
  - ライブラリ側は、ログが実際にどのように出力されるか、を気にする必要はない
  - また、そのような実処理は[logi_stdlib](https://github.com/sile/logi_stdlib)等の外部リポジトリに委譲し、`logi`自体は安定したインタフェースの提供のみに集中している
    - `logi`も最低限のログ出力を行うための機能は備えているが、動作確認目的であって、実運用に耐え得るものではない
- 各種設定や構成の更新が動的かつ柔軟に行える
  - 実行中に簡単かつ安全に、ログレベルの変更等が実施できる
- ログ出力先の切替が可能
  - e.g., アプリケーションログ と アクセスログ は別々のファイルに出力する
- ログメッセージの共通ヘッダを設定するための仕組みを提供
  - ログコンテキストオブジェクトを持ち回すことで、gen_event等でハンドラモジュールごとに異なるヘッダの管理が可能
- (仕組みとしては)スケーラブル
  - `logi`レベルでは、極力単一のボトルネックが生じないような設計および実装となっている
  - 例えば巨大なログ出力によりスローダウンしているプロセスがあったとしても、それによって他のログ出力プロセスが
  - 最終的には、シンク等の実装モジュール次第ではある
- ある程度柔軟なカスタマイズが可能
  - フィルタやレイアウト、シンク(ログ出力)、といったコンポーネントがビヘイビアとして提供されており、独自実装が用意
- ロガーインスタンスのスコープの自由度が高い
  - 例えば同じヘッダやフィルタを持つロガーを、複数プロセス間で受け渡す、といったことが可能
  - 異なる設定のロガーを同一プロセス内で使い分けることも可能
    - gen_event等のように、同じプロセスに複数のハンドラが相乗りする場合に有用になり得る

ビルド方法
----------

ビルドツールには[rebar3](https://github.com/erlang/rebar3)を使用している。

プロジェクトルートの`rebar.config`ファイルに以下を記載することで、logiが利用可能となる。

```erlang
%% rebar.config

%% ...他の項目は省略...

{erl_opts,
  [
    %% logiは、関数名や行番号の取得用にparse_transformを使っているので、コンパイルオプションで有効にする
    {parse_transform, logi_transform}
  ]}.

{deps,
  [
    %% 依存アプリケーションにlogiを追加する
    %% (必要に応じて logi_stdlib 等も依存に追加する。使用例も参照のこと)
    logi
  ]}.
```

自アプリケーションの起動時に、logiも起動するように`.app.src`ファイルも修正しておく。

```erlang
%% src/${APPLICATION}.app.src
{application, ${APPLICATION},
  [
    %% ...他の項目は省略...
    {applications,
       [
         kernel,
         stdlib,
         logi  % 依存に追加
       ]}
  ]}.
```

以下のようにErlangシェル上で、ログ出力を試すことも可能。

```erlang
$ rebar3 shell
> application:start(logi).

%% parse_transformを使わない場合に出力される警告を抑制するためのお約束
> error_logger:tty(false).

%% 組み込みのシンクをインストール
> logi_channel:install_sink(logi_builtin_sink_io_device:new(hoge), info).

%% ログ出力
> logi:info("Hello World!").
2016-05-08 11:34:37.642 [info] nonode@nohost <0.82.0> erl_eval:do_apply:674 [] Hello World!
```

登場人物
--------

### ロガー

`logi`モジュール:
- ログ出力関連の関数をまとめたモジュール
- 主にロガー(ログ出力クライント)の操作用の関数を提供する
- 通常のログ出力用途であれば、このモジュールだけを参照すれば大半は事足りる
- ログメッセージには、ヘッダやメタデータを付与可能:
  - ヘッダやメタデータをどのように利用するかは、後述のシンク次第
  - 一般的には、ヘッダには最終的な出力的に含まれる情報を、メタデータにはそれ以外の情報を、設定することが推奨される
- `logi:set_headers/1`等の関数を使うことで、複数ログ出力に共通のヘッダ等を設定可能
  - デフォルトではプロセス辞書に保存される (スコープはプロセス単位)
  - 明示的にロガーインスタンスを持ち回すことも可能
    - e.g., 複数のロガーを併用したい場合、プロセスを跨いでヘッダ等を持ち回したい場合

`logi_filter`モジュール(ビヘイビア):
- 各ログメッセージの出力可否を制御するためにビヘイビア
- ロガー単位で任意のフィルタが設定可能

### チャンネル

`logi_channel`モジュール:
- ログメッセージの論理的な一次出力先
- 各チャンネルには、任意個のシンクがインストール可能
  - 各シンクは`logi_condition`によって指定された適用条件を有する
- チャンネルに出力されたメッセージは、適用条件を満たしたシンク群に供給される
  - この処理は、クライアントサイドのみで完結する
    - ログ出力が集中した場合でも、チャンネルプロセスがボトルネックとなることはない
    - チャンネル単位のETSに対する読み込み要求は発生する

### シンク

`logi_sink`モジュール:
- シンク:
  - 各ログメッセージを、実際にどのように出力するかハンドリングするコンポーネント
  - 特定のチャンネルにインストールされ、そこに送られたログメッセージ群を受け取り、外部に出力する
- チャンネルへのインストール時に、対応するプロセスが生成される
  - このプロセスは、該当シンクや対応するWriterインスタンスの管理を行う
  - `logi_sink_proc`モジュールや`logi_sink_writer`モジュール(ビヘイビア)も参照
- Writerインスタンスには、ログ出力の度に、`logi_context`オブジェクトとログメッセージが渡される
  - `logi_sink_writer`の実装モジュール次第で、それが(例えば)標準出力やファイルに出力されたりする
- 必須ではないが`logi_layout`ビヘイビアの実装モジュールのインスタンスを、初期化時に受け取るシンクも多い
  - 各ログメッセージのフォーマット(i.e., 外部出力可能な文字列への変換)を行うのは、このビヘイビアの責務
- シンク自体やその適用条件は、動的に変更が可能

---

他のOTPアプリケーションから利用されるライブラリでは、基本的にロガー(i.e., `logi`モジュール)のみを利用することが推奨される
(各ログメッセージが、実際にどのように出力されるのか、には関知しない方が望ましい)。

それらのライブラリ群を利用するルートとなるアプリケーション(システム)では、
その起動時に、チャンネルおよびシンクのセットアップを行い、各ログメッセージがどのように出力されるべきなのかを指定する。

なお、実システムでの利用に耐え得る、シンクや各種ビヘイビアの実装に関しては
[logi_stdlib](https://github.com/sile/logi_stdlib)等の外部リポジトリを参照のこと
(`logi`自体は、サンプル程度のモジュール群しか提供していない)。


ログレベルについて
------------------

以下の[Syslog相当](http://tools.ietf.org/html/rfc5424#section-6.2.1)の重大度(severity)を採用している。

| severity  | description                      |
|-----------|----------------------------------|
| debug     | debug-level messages             |
| info      | informational messages           |
| notice    | normal but significant condition |
| warning   | warning conditions               |
| error     | error conditions                 |
| critical  | critial conditions               |
| alert     | action must be taken immediately |
| emergency | system is unusable               |

各重大度の使用の目安としては[ロギングとデバッグのコンフィグレーション](http://otndnld.oracle.co.jp/document/products/wlevs/docs30/config_server/logging.html#wp1013043)のページも参考となる。

API
---

[Edocドキュメント](doc/README.md)を参照。


使用例: ライブラリの場合
------------------------

ここでの「ライブラリ」とは「他のアプリケーションから利用されるようなOTPアプリケーション」のことを指す。

ライブラリでは、基本的にはログ出力系の関数の使用が主となる想定。

```erlang
-module(hello).

-export([world/0]).

world() ->
  logi:info("Hello World!"), % 実際にどのように出力されるかは、使用されているシンク次第
  ok.
```

独自のヘッダやメタデータをメッセージに付与することも可能。

```erlang
-module(hello).

-export([hello/3]).

hello(Id, Meta, Name) ->
  %% 後続のログ出力用に、ヘッダとメタデータを(デフォルトロガーに)設定する (この形式ではプロセス辞書に保存される)。
  %% これらがどのように用いられるかは、使用されているフィルタやシンク次第。
  logi:set_headers(#{id => Id}),
  logi:set_metadata(#{meta => Meta}),

  logi:info("Hello ~p!", [Name]),
  ok.
```

ライブラリプロセスがロガーインスタンスを受け取るようにしておけば、
利用者側のコンテキスト(e.g., ヘッダやフィルタ、出力先チャンネル)を引き継ぐことが可能。

```erlang
-module(hoge_server).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% プロセス起動時に、ロガーインスタンスを受け取るようにしておく
-spec start_link(logi:logger()) -> {ok, pid()} | {error, term()}.
start_link(Logger) ->
  gen_server:start_link(?MODULE, [Logger], []).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  start_link(logi:default_logger()). % デフォルトロガー

init([Logger]) ->
  %% デフォルトロガーとして(プロセス辞書に)保存して、明示的なインスタンスの持ち回しの手間を省く
  logi:save_as_default(Logger),
  logi:info("Hoge server is started"), % `Logger`に設定されているヘッダ等は、そのまま引き継がれる
  {ok, []}.
```

同一プロセス内で複数のロガーを併用したい場合。

```erlang
-module(hello).

-export([world/0]).

world() ->
  %% 出力チャンネルを変えたり、ヘッダを変えたり等、複数のロガーを使いたい場合
  %% (下記のチャンネルは`logi_channel:create/1`によって、既に作成済みのものとする)
  HelloLogger0 = logi:new([{channel, hello_log}]),
  WorldLogger0 = logi:new([{channel, world_log}]),

  %% 明示的に持ち回す場合には、ロガーを返す関数の結果を捨てずにハンドリングする必要がある
  HelloLogger1 = logi:info("Hello", [], [{logger, HelloLogger0}]),
  WorldLogger1 = logi:info("World!", [], [{logger, WorldLogger0}]),

  %% プロセス辞書に保存することも可能
  %% (ロガー更新系の関数呼び出し時に、自動で再保存されるので、こちらの方が利便性は高い)
  logi:save(hello_log, HelloLogger1),
  logi:save(world_log, WorldLogger1),

  logi:info("Hello", [], [{logger, hello_log}]),
  logi:info("World!", [], [{logger, world_log}]),

  ok.
```

使用例: アプリケーションの場合
------------------------------

ここでの「アプリケーション」とは「他に依存(利用)されることなく、システムのルートとなるOTPアプリケーション」のことを指す。

ログ出力に関しては、基本的には上述のライブラリの場合と同様だが、
ルートアプリケーションの場合には、以下のようにチャンネルやシンクの構成設定を行い、
具体的にどのようにログが外部に出力されるのかを指定する必要がある。

```erlang
-module(system_boot).

-export([init_log_config/0]).
-export([update_log_config/0]).

%% 起動時
init_log_config() ->
  %% 必要なチャンネルを作成する
  ok = logi_channel:create(application_log),
  ok = logi_channel:create(console_log),

  %% チャンネルにシンク(群)を登録する
  %% (ここでは便宜上、組み込みシンクを利用しているが、実際にはlogi_stdlib等が提供するものを利用するのが望ましい)
  {ok, Fd} = file:open("/tmp/application.log", [write]),
  FileSink = logi_builtin_sink_io_device:new(file_sink, [{io_device, Fd}]),
  {ok, _} = logi_channel:install_sink(application_log, FileSink, info),

  ConsoleSink = logi_builtin_sink_io_device:new(console_sink),
  {ok, _} = logi_channel:install_sink(console_log, ConsoleSink, debug),

  ok.

%% 設定更新時
update_log_config() ->
  %% 保存先ファイルを変更する
  {ok, Fd} = file:open("/tmp/application_2.log", [write]),
  FileSink = logi_builtin_sink_io_device:new(file_sink, [{io_device, Fd}]),
  {ok, _} = logi_channel:install_sink(application_log, FileSink, info),

  %% ログレベルを変更する (`logi_channel:install_sink/3`でも可)
  {ok, _} = logi_channel:set_sink_condition(console_log, console_sink, alert),

  ok.
```

シンクの適用条件の指定
----------------------

単純なログレベルによる指定(e.g., "info以上の重大度のもののみ出力する”だけではなく、以下のような、より細かい指定が可能:
- 特定の重大度のメッセージにのみ適用 (e.g., "alertログにのみ適用")
- 重大度の範囲指定 (e.g, "debugからnoticeまでに適用")
- 重大度の個別指定 (e.g, "infoとwarningに適用")
- 出力元アプリケーションやモジュールの指定 (e.g., "stdlibアプリケーションからのログにのみ適用")

詳細は`logi_condition`モジュールのドキュメントを参照のこと。

より細かい条件で出力を制御したい場合には、フィルタやシンクで行うこと。

ライセンス
----------

MITライセンス。

詳細は[LICENSE](LICENSE)ファイルを参照。
