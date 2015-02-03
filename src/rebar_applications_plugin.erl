%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc アプリケーションリソースファイル(*.app)の`applications'設定を自動で生成するためのプラグイン
%%
%% 詳細な動作に関しては README.md を参照
-module(rebar_applications_plugin).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export(['fill-apps'/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(OPTION(Key, Config, Default), proplists:get_value(Key, rebar_config:get_local(Config, fill_apps_opts, []), Default)).
-define(DEBUG(Format, Args), rebar_log:log(debug, "[~p:~s:~p] "++Format++"~n", [self(), ?MODULE, ?LINE | Args])).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 対象アプリケーションの実際の依存関係を解析して、アプリケーションリソースファイルの`applications'項目を自動生成する
-spec 'fill-apps'(rebar_config:config(), filename:name()) -> ok | {ok, rebar_config:config()}.
'fill-apps'(_Config, undefined) ->
    ok; % アプリケーションリソースファイルが存在しない場合は対象外
'fill-apps'(Config00, AppFile) ->
    case rebar_app_utils:is_app_src(AppFile) of
        false -> ok;
        true  ->
            ?DEBUG("appfile=~s", [AppFile]),

            Config0 = init_xref(Config00),
            {Config1, AppName} = rebar_app_utils:app_name(Config0, AppFile),
            {Config2, Applications0} = rebar_app_utils:app_applications(Config1, AppFile),
            ?DEBUG("appname=~s", [AppName]),

            Applications1 = collect_direct_depending_applications(AppName, AppFile, Config2),
            Applications2 = update_applications(Applications0, Applications1),
            ?DEBUG("apps: original=~w, collected=~w, result=~w", [Applications0, Applications1, Applications2]),

            ok = rewrite_applications(AppFile, Applications2),
            {ok, Config2}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
init_xref(Config0) ->
    case xref:start(?MODULE, [{xref_mode, modules}]) of
        {error, {already_started, Xref}} ->
            ?DEBUG("xref is already started: pid=~p, name=~s", [Xref, ?MODULE]),
            Config0;
        {ok, Xref} ->
            ?DEBUG("start new xref: pid=~p, name=~s", [Xref, ?MODULE]),

            SystemLibDir = code:lib_dir(),
            IncludeSystemApps = ?OPTION(include_system_apps, Config0, false),

            Config1 =
                lists:foldl(
                  fun (EbinPath, AccConfig0) ->
                          case rebar_app_utils:is_app_dir(filename:dirname(EbinPath)) of
                              false           -> AccConfig0;
                              {true, AppFile} ->
                                  {AccConfig1, AppName} = rebar_app_utils:app_name(AccConfig0, AppFile),
                                  _ = case xref:add_application(Xref, filename:dirname(EbinPath), [{name, AppName}]) of
                                          {ok, App} -> ?DEBUG("xref added: app=~s", [App]);
                                          _         -> ok
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

-spec collect_direct_depending_applications(atom(), filename:name(), rebar_config:config()) -> [atom()].
collect_direct_depending_applications(AppName, AppFile, Config) ->
    {ok, Calls0} = xref:analyze(?MODULE, {application_call, AppName}), % XXX: MODULE
    Calls = ordsets:subtract(ordsets:from_list(Calls0), get_subapps(Config)),
    case rebar_app_utils:is_app_dir() of
        {true, AppFile} -> lists:delete(AppName, Calls ++ get_subapps(Config)); % root application
        {true, _}       -> lists:delete(AppName, Calls)
    end.

%% TODO: cache
get_subapps(Config) ->
    Dirs = rebar_config:get_local(Config, sub_dirs, []),
    ordsets:from_list(
      lists:filtermap(
        fun (Dir) ->
                case rebar_app_utils:is_app_dir(Dir) of
                    false           -> false;
                    {true, AppFile} ->
                        {_, AppName} = rebar_app_utils:app_name(Config, AppFile),
                        {true, AppName}
              end
        end,
        Dirs)).

update_applications(Apps0, Apps1) ->
    Apps0 ++ lists:foldl(fun lists:delete/2, Apps1, Apps0).

rewrite_applications(AppFile, Applications) ->
    EbinAppFile = rebar_app_utils:app_src_to_app(AppFile),
    {ok, [{application, AppName, AppKeys0}]} = file:consult(EbinAppFile),
    AppKeys1 = lists:keystore(applications, 1, AppKeys0, {applications, Applications}),
    file:write_file(EbinAppFile, io_lib:format("~p.\n", [{application, AppName, AppKeys1}])).
