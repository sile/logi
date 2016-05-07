logi
====

Erlangでログ出力を行うためのインタフェースを提供するライブラリ。

主にサーバライブラリに対して軽量かつ可搬なログ出力方法を提供するのが目的。

特徴
----

TODO:

- ログ出力のためのインターフェース関数のみを提供
  - ライブラリ側は、ログが実際に、どこに／どのように、出力されるか、を
- バックエンドの登録／削除が柔軟に行える
  - 設定切替時のログ損失なし
- ログ出力先の切替が可能
  - e.g., アプリケーションログ と アクセスログ は別々のファイルに出力する
- ログメッセージの共通ヘッダを設定するための仕組みを提供
  - ログコンテキストオブジェクトを持ち回すことで、gen_event等でハンドラモジュールごとに異なるヘッダの管理が可能
- (仕組みとしては)スケーラブルに

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

TODO

TODO: ヘッダとメタデータの使い分け

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
  %% 後続のログ出力用に、ヘッダとメタデータを設定する (この形式ではプロセス辞書に保存される)。
  %% これらがどのように用いられるかは、使用されているシンク次第。
  logi:set_headers(#{id => Id}),
  logi:set_metadata(#{meta => Meta}),

  logi:info("Hello ~p!", [Name]),
  ok.
```

ロガーインスタンスを受け取る場合

複数使いたい場合

使用例: アプリケーションの場合
------------------------------

OTP用語ではなく、一般的な

使用方法
--------
とりあえず現段階での使用方法:

rebar.configにlogiおよびlogi_ttyを追加する
```erlang
{deps,
  [
    {logi, ".*", {git, "git://github.com/sile/logi.git", {branch, "master"}}},
    {logi_tty, ".*", {git, "git://github.com/sile/logi_tty.git", {branch, "master"}}}
  ]}.
```

アプケーションの開始および標準出力バックエンドのインストール
```erlang
> application:ensure_all_started(logi).
> application:ensure_all_started(logi_tty).

%% 標準出力にログが出力されるようにする
> logi_tty:install(info).
```

使用例
------

### 1. 基本的な使用方法

### 2. ログ出力先の変更

### 3. ログコンテキストの制御

### 4. 特定のアプリケーションだけログレベルを変更する方法

メモ
----

### アドホックかつ詳細なトレースを行いたい場合

- Erlangのトレース機能を使用するべき (ex. recon)

### 想定する構成

- ライブラリ
  - logiのみ
- アプリケーション
  - 必要に応じて、以下を使う
     - logi_fileやlogi_tty 等の出力用バックエンド
     - logi_lager, lager_logi, error_logger_logi等の転送用ライブラリ (NOTE: 存在しないものもある)

### バックエンドの適用条件

- min-level, max-level
- location: application, module, function, line, process
- header, metadata (?)
- arbitrary predicate function (??)
- and, or, not

- 制約は、全体の性能劣化に繋がらないもののみにしたい (関係箇所以外には影響なし)
  - それ以外は、条件付きで中継するようなバックエンドをdebugレベルで登録すれば実現可能
  - 組み込みと性能は変わらず、柔軟性は高い
  - logi_backend_conditionalとか提供しても良い (application, module, funtion, etc...)
- max_levelはok
- `sys:replace_state/3`を使って、特定プロセスに直接バックエンドを指定するのはありかもしれない


- 推奨: ライブラリがloggerを受け取れるようにして、利用者側が各種設定を制御可能にする


```
        -- logger --- channel --- sink
        |
source -|- logger --- channel --- sink
                   |           |
source --- logger --           -- sink
```

ライセンス
----------

MITライセンス。

詳細は[LICENSE](LICENSE)ファイルを参照。
