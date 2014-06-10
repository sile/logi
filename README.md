logi
====

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

TODO
----
ちゃんとしたドキュメント
