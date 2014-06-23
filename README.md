logi
====

Erlangでログ出力を行うためのインタフェースを提供するライブラリ。  
主にサーバライブラリに対して軽量かつ可搬なログ出力方法を提供するのが目的。

特徴
----

- ログ出力のためのインターフェース関数のみを提供
- バックエンドの登録／削除が柔軟に行える
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

TODO
----
ちゃんとしたドキュメント
