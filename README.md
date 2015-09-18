logi
====

Erlangでログ出力を行うためのインタフェースを提供するライブラリ。
主にサーバライブラリに対して軽量かつ可搬なログ出力方法を提供するのが目的。

特徴
----

- ログ出力のためのインターフェース関数のみを提供
- バックエンドの登録／削除が柔軟に行える
  - 設定切替時のログ損失なし
- ログ出力先の切替が可能
  - ex. アプリケーションログ と アクセスログ は別々のファイルに出力する
- ログメッセージの共通ヘッダを設定するための仕組みを提供
  - ログコンテキストオブジェクトを持ち回すことで、gen_event等でハンドラモジュールごとに異なるヘッダの管理が可能
- 各ログの出力頻度の制御が可能 (experimental)

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

API
---

[EDoc](doc/README.md)を参照

TODO
----
ちゃんとしたドキュメント

メモ
----

### 頻度制御について

- 期間を無制限にする場合の注意
  - ゴミが残る可能性がある
  - 特に、gen_event実装的なモジュールで、ID を指定した場合
- 省略メッセージの出力タイミング
  - 同一プロセスの何らかのメッセージ出力時に、省略期間を超過していたら
  - プロセスが(次のログを出さずに)死んだら出力されない可能性がある
- より高度の制御が欲しい場合は?
  - ユーザが自前で独自の機構を実装すれば良い (TODO: 例)

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

### 推奨: xref.deprecated_function_calls

`logi_transform` の設定ミスを防ぐため

### layout周りの性能ヒント

`{Module, Options}`としてオプション群を渡すよりも、
`format(...) -> Module:format(..., Options).`となるような独自レイアウトを定義した方が性能的には有利。
