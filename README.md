rebar_applications_plugin
=========================

アプリケーションリソースファイル(.app)内の`applications`項目を自動で生成するための[rebar](https://github.com/rebar/rebar)プラグイン。

なお`applications`は、そのアプリケーションが依存するアプリケーション群を指定するための項目。

参照: http://www.erlang.org/doc/man/app.html


使い方
------

[rebar.config](https://github.com/rebar/rebar/blob/master/rebar.config.sample)に以下の項目を追加することで、このプラグインを有効にできる。<br />
※ なお、このプラグインはアプリケーションのコンパイル後に自動で適用される。
```erlang
%%%
%%% in rebar.conrig
%%%

%% プラグインのソースが配置されているディレクトリを指定する
{plugin_dir, "deps/rebar_applications_plugin/src"}.

%% 使用するプラグインを指定する
{plugins, [rebar_applications_plugin]}.

%% このプラグインに指定可能なオプション (省略可)
%% - include_system_lib:
%%   - code:lib_dir()以下に配置されているライブラリ群を依存性解析対象に含めるかどうか
%%   - 含めた場合、処理時間が長くなってしまうので、デフォルトはfalse
{fill_apps_opts, [{include_system_lib, false}]}.

%% 依存ライブラリに追加する
{deps,
  [
   {rebar_applications_plugin, ".*",
     {git, "https://github.com/sile/rebar_applications_plugin", {tag, "v0.1.0"}}}
  ]}.
```

補足: `applications`項目の役割
------------------------------

アプリケーションリソースファイル(*.app)内の`applications`項目は、
そのアプリケーションが依存しているアプリケーション群を記述するための項目で、
主に以下の用途で使用される。
- `application:start/1`呼び出し時の依存性チェック
  - 上記関数(およびその派生関数群)は、指定されたアプリケーションの起動時に、それが依存しているアプリケーション群が全て起動済みであることを要求する
  - `application:ensure_all_started/1`を使うと、未起動の依存アプリケーションがあれば、(再帰的に)それらを全て起動した上で、指定アプリケーションを起動してくれるので便利
- リリース物に含めるアプリケーション群の決定
  - リリース管理ツールである[reltool](http://www.erlang.org/doc/man/reltool.html)の設定ファイル(reltool.config)では`rel`項目でリリースパッケージに含めるアプリケーション群を指定する
  - そこで指定されたアプリケーションおよび、それが(再帰的に)依存する全てのアプリケーションがリリースパッケージに含まれることになる
  - `applications`項目が適切に記述されていれば、ルートとなるアプリケーションだけを指定すれば良くなるので、依存アプリケーションの増減に伴う記述修正の手間が減る

なお、複数アプリケーション間に循環する依存関係が指定されている場合は`application:ensure_all_started/1`の呼び出し時等に、無限ループとなってしまうので注意が必要。<br />


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


内部動作
--------

TODO: 生成ルールと統合

TODO: applicatinの役割(ensure, release, curcle)
