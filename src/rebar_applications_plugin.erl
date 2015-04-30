%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc アプリケーションリソースファイル(*.app)の`applications'設定を自動で生成するためのプラグイン
%%
%% 詳細な動作に関しては README.md を参照
-module(rebar_applications_plugin).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([post_compile/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(OPTION(Key, Config, Default), proplists:get_value(Key, rebar_config:get(Config, fill_apps_opts, []), Default)).
-define(DEBUG(Format, Args), rebar_log:log(debug, "[~s:~p] "++Format++"~n", [?MODULE, ?LINE | Args])).
-define(XREF_SERVER, ?MODULE).
-define(APPS_CACHE_KEY, {?MODULE, apps_cache}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 対象アプリケーションの実際の依存関係を解析して、アプリケーションリソースファイルの`applications'項目を自動生成する
-spec post_compile(rebar_config:config(), filename:name()) -> ok | {ok, rebar_config:config()}.
post_compile(Config0, AppFile) ->
    case AppFile =/= undefined andalso rebar_app_utils:is_app_src(AppFile) of
        false -> ok; % `AppFile'が"*.app.src"ではない場合は対象外
        true  ->
            case is_deps_app(AppFile, Config0) of
                true  -> ok; % 依存アプリケーションの場合は対象外
                false ->
                    ?DEBUG("appfile=~s", [AppFile]),

                    Config1 = init_xref(Config0),
                    Config2 = add_current_app_to_xref(AppFile, Config1),
                    {Config3, AppName} = rebar_app_utils:app_name(Config2, AppFile),
                    {Config4, Applications0} = rebar_app_utils:app_applications(Config3, AppFile),
                    ?DEBUG("appname=~s", [AppName]),

                    Applications1 = collect_direct_depending_applications(AppName, Config4),
                    Applications2 = merge_applications(Applications0, Applications1),
                    ?DEBUG("apps: original=~w, collected=~w, result=~w", [Applications0, Applications1, Applications2]),

                    ok = rewrite_applications(AppFile, Applications2),
                    {ok, Config4}
            end
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc xrefサーバを起動して、依存関係解析のための下準備を整える
%%
%% 毎回、準備処理が走るのを避けるために、このサーバは一度起動したらそのままにしておく(rebarの終了時に自動で落ちる)
-spec init_xref(rebar_config:config()) -> rebar_config:config().
init_xref(Config0) ->
    case xref:start(?XREF_SERVER, [{xref_mode, modules}]) of
        {error, {already_started, Xref}} ->
            ?DEBUG("xref is already started: pid=~p", [Xref]),
            Config0;
        {ok, Xref} ->
            ?DEBUG("start new xref: pid=~p", [Xref]),

            %% `{include_system_apps, false}'がオプションで指定されている場合は、システムライブラリ群は解析対象に含めない
            %% (所要時間が長くなるので、デフォルトは`false')
            SystemLibDir = code:lib_dir(),
            IncludeSystemApps = ?OPTION(include_system_apps, Config0, false),
            Excludes0 = ?OPTION(exclude_apps, Config0, []),

            %% ルートおよびサブアプリケーション群は、初期登録からは除外する
            %% (コンパイル前の不完全な状態で登録されてしまうのを避けるため)
            Excludes = gb_sets:from_list(get_root_and_subapps(Config0) ++ Excludes0),

            Config1 =
                lists:foldl(
                  fun (EbinPath, AccConfig0) ->
                          case rebar_app_utils:is_app_dir(filename:dirname(EbinPath)) of
                              false           -> AccConfig0;
                              {true, AppFile} ->
                                  {AccConfig1, AppName} = rebar_app_utils:app_name(AccConfig0, AppFile),
                                  case gb_sets:is_member(AppName, Excludes) of
                                      true  -> ok;
                                      false ->
                                          Result = xref:add_application(Xref, filename:dirname(EbinPath), [{name, AppName}]),
                                          ?DEBUG("xref added: result=~w", [Result])
                                  end,
                                  AccConfig1
                          end
                  end,
                  Config0,
                  lists:filter(fun (EbinPath) ->
                                       IncludeSystemApps orelse (not lists:prefix(SystemLibDir, EbinPath))
                               end,
                               code:get_path())),
            Config1
    end.

-spec add_current_app_to_xref(filename:name(), rebar_config:config()) -> rebar_config:config().
add_current_app_to_xref(AppFile, Config0) ->
    {Config1, AppName} = rebar_app_utils:app_name(Config0, AppFile),
    Result = xref:add_application(?XREF_SERVER, filename:dirname(filename:dirname(AppFile)), [{name, AppName}]),
    ?DEBUG("xref added: result=~w", [Result]),
    Config1.

%% @doc xrefを使って`AppName'が直接依存(使用)しているアプリケーション群を取得する
-spec collect_direct_depending_applications(AppName, rebar_config:config()) -> ordsets:ordset(AppName) when
      AppName :: atom().
collect_direct_depending_applications(AppName, Config) ->
    {ok, Calls0} = xref:analyze(?XREF_SERVER, {application_call, AppName}),
    Calls1 = ordsets:from_list(Calls0),
    case rebar_utils:processing_base_dir(Config) of
        false -> ordsets:del_element(AppName, Calls1);
        true  ->
            %% ルートアプリケーションは全てのサブアプリケーションを依存に含める
            Includes = get_root_and_subapps(Config),
            ?DEBUG("includes: ~w", [Includes]),
            ordsets:del_element(AppName, ordsets:union(Calls1, Includes))
    end.

-spec get_root_and_subapps(rebar_config:config()) -> ordsets:ordset(atom()).
get_root_and_subapps(Config) ->
    case get(?APPS_CACHE_KEY) of
        undefined ->
            BaseDir = rebar_utils:base_dir(Config),
            Dirs = ["" | rebar_config:get(Config, sub_dirs, [])],
            Apps =
                ordsets:from_list(
                  lists:filtermap(
                    fun (Dir) ->
                            case rebar_app_utils:is_app_dir(filename:join(BaseDir, Dir)) of
                                false           -> false;
                                {true, AppFile} ->
                                    {_, AppName} = rebar_app_utils:app_name(Config, AppFile),
                                    {true, AppName}
                            end
                    end,
                    Dirs)),
            put(?APPS_CACHE_KEY, Apps), % キャッシュする
            Apps;
        Apps -> Apps
    end.

-spec merge_applications([atom()], ordsets:ordset(atom())) -> [atom()].
merge_applications(OrignalApps, GeneratedApps) ->
    OrignalApps ++ ordsets:subtract(GeneratedApps, ordsets:from_list(OrignalApps)).

-spec rewrite_applications(filename:name(), [atom()]) -> ok | {error, Reason::term()}.
rewrite_applications(AppFile, Applications) ->
    EbinAppFile = rebar_app_utils:app_src_to_app(AppFile),
    {ok, [{application, AppName, AppKeys0}]} = file:consult(EbinAppFile),
    AppKeys1 = lists:keystore(applications, 1, AppKeys0, {applications, Applications}),
    file:write_file(EbinAppFile, io_lib:format("~p.\n", [{application, AppName, AppKeys1}])).

-spec is_deps_app(filename:name(), rebar_config:config()) -> boolean().
is_deps_app(AppFile, Config) ->
    case rebar_deps:get_deps_dir(Config) of
        {true, DepsDir} -> lists:prefix(DepsDir, AppFile);
        _               -> false
    end.
