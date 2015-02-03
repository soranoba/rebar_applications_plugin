rebar_applications_plugin
=========================

アプリケーションリソースファイル(.app)内の`applications`項目を自動で生成するためのrebarプラグイン。

なお`applications`は、そのアプリケーションが依存するアプリケーション群を指定するための項目。

参照: http://www.erlang.org/doc/man/app.html


想定するアプリケーション構成
----------------------------

```bash
- ROOT_APP       # ルートアプリケーション
  - rebar.config
  - apps         # サブアプリケーション群: オプショナル（具体的なパスはrebar.config内で変更可能）
    - SUB_APP_1
    - SUB_APP_N
  - deps         # 依存アプリケーション群: オプショナル（具体的なパスはrebar.config内で変更可能）
    - DEP_APP_1
    - DEP_APP_N
```

生成ルール
----------

- rebarの流儀に従い`src/APP.app.src`をもとにして`ebin/APP.app`を生成する
- `src/APP.app.src`の`applications`項目で既に指定されているアプリケーション群は、そのまま使用される
  - 自動生成された依存アプリケーションのリストは、その後ろに(重複が除去された上で)付け足される
- TODO: 依存関係の判定方法
- TODO: サブアプリケーションと依存アプリケーションの扱い、注意点

使い方
------

```erlang
{plugin_dir, "deps/rebar_applications_plugin/src"}.

{plugins, [rebar_applications_plugin]}.

{fill_apps_opts, [{include_system_lib, false}]}.

{rebar_applications_plugin, ".*",
    {git, "https://github.com/sile/rebar_applications_plugin", {tag, "v0.1.0"}}}
```

TODO: 複数のプラグインを使いたい場合の注釈

内部動作
--------

TODO: 生成ルールと統合
