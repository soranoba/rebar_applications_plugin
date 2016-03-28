%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc アプリケーションリソースファイル(*.app)の`applications'設定を自動で生成するためのプラグイン
%%
%% 詳細な動作に関しては README.md を参照
-module(rebar_applications_plugin).
-behaviour(provider).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, do/1, format_error/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(DEBUG(Format, Args), rebar_log:log(debug, "[~s:~p] "++Format++"~n", [?MODULE, ?LINE | Args])).
-define(XREF_SERVER, ?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

init(State) ->
    Provider = providers:create([{name, generate},
                                 {module, ?MODULE},
                                 {namespace, auto_app_src},
                                 {bare, true},
                                 {short_desc, ""},
                                 {deps, [{default, lock}]},
                                 {desc, ""},
                                 {opts, [{exclude_apps,        undefined, exclude, binary},
                                         {include_system_apps, undefined, all,     boolean}]}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Apps = rebar_state:project_apps(State),
    ?DEBUG("apps = ~p", [ [rebar_app_info:name(App) || App <- Apps] ]),
    ok = init_xref(State),
    lists:foreach(fun(App) ->
                          Applications = collect_direct_depending_applications(App, State),
                          rewrite_applications(App, Applications)
                  end, Apps),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc xrefサーバを起動して、依存関係解析のための下準備を整える
%%
%% 毎回、準備処理が走るのを避けるために、このサーバは一度起動したらそのままにしておく(rebarの終了時に自動で落ちる)
init_xref(State) ->
    case xref:start(?XREF_SERVER, [{xref_mode, modules}]) of
        {error, {already_started, Xref}} ->
            ?DEBUG("xref is already started: pid=~p", [Xref]),
            ok;
        {ok, Xref} ->
            ?DEBUG("start new xref: pid=~p", [Xref]),

            %% `{include_system_apps, false}'がオプションで指定されている場合は、システムライブラリ群は解析対象に含めない
            %% (所要時間が長くなるので、デフォルトは`false')
            {Args, _}       = rebar_state:command_parsed_args(State),
            IsAll           = proplists:get_value(include_system_apps, Args, false),
            ExcludeAppNames = binary:split(proplists:get_value(exclude_apps, Args, <<>>), <<",">>, [global]),

            ?DEBUG("include_system_apps = ~p", [IsAll]),
            ?DEBUG("exclude_apps = ~p", [ExcludeAppNames]),
            AllApps = rebar_state:all_deps(State) ++ rebar_state:project_apps(State),
            ?DEBUG("all apps = ~p", [ [rebar_app_info:name(App) || App <- AllApps] ]),

            lists:foreach(fun(App) ->
                                  case lists:member(AppName = rebar_app_info:name(App), ExcludeAppNames) of
                                      true  -> ok;
                                      false ->
                                          ?DEBUG("add : ~p (~s)", [AppName, rebar_app_info:dir(App)]),
                                          ?DEBUG("result = ~p", [xref:add_application(Xref, rebar_app_info:dir(App), [{name, binary_to_atom(AppName, utf8)}])])
                                  end
                          end, AllApps)
    end.

%% @doc xrefを使って`AppName'が直接依存(使用)しているアプリケーション群を取得する
collect_direct_depending_applications(App, State) ->
    {ok, Calls0} = xref:analyze(?XREF_SERVER, {application_call, AppName = binary_to_atom(rebar_app_info:name(App), utf8)}),
    ?DEBUG("anaylze result = ~p", [Calls0]),
    Calls1 = ordsets:from_list(Calls0),
    case (ProjectApps = rebar_state:project_apps(State)) =/= [] andalso hd(ProjectApps) =:= App of
        false -> ordsets:to_list(ordsets:del_element(AppName, Calls1));
        true  ->
            %% ルートアプリケーションは全てのサブアプリケーションを依存に含める
            Includes = [binary_to_atom(rebar_app_info:name(I), utf8) || I <- ProjectApps],
            ?DEBUG("includes: ~w", [Includes]),
            ordsets:to_list(ordsets:del_element(AppName, ordsets:union(Calls1, ordsets:from_list(Includes))))
    end.

rewrite_applications(App, Applications) ->
    AppFile = rebar_app_info:app_file(App),
    ?DEBUG(".app = ~s", [AppFile]),
    {ok, [{application, AppName, AppKeys0}]} = file:consult(AppFile),
    AppKeys1 = lists:keystore(applications, 1, AppKeys0, {applications, Applications}),
    file:write_file(AppFile, io_lib:format("~p.\n", [{application, AppName, AppKeys1}])).
