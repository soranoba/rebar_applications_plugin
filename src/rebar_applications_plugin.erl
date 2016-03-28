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
                                 {short_desc, ""},
                                 {desc, ""},
                                 {opts, [{exclude_apps,        undefined, exclude, binary},
                                         {include_system_apps, undefined, all,     boolean}]}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Apps = [rebar_state:current_app(State) | rebar_state:project_apps(State)],
    ok = init_xref(State),
    lists:foreach(fun(App) ->
                          AppNameAtom  = binary_to_atom(rebar_app_info:name(App), utf8),
                          Applications = collect_direct_depending_applications(AppNameAtom, State),
                          AppFile      = rebar_app_info:app_file(App),
                          rewrite_applications(AppFile, Applications)
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
            SystemLibDir    = code:lib_dir(),
            {Args, _}       = rebar_state:command_parsed_args(State),
            IsAll           = proplists:get_value(include_system_apps, Args, false),
            ExcludeAppNames = binary:split(proplists:get_value(exclude_apps, Args, <<>>), <<",">>, [global]),

            DepEbinAndApps  = [{filename:absname(rebar_app_info:ebin_dir(App)), App}
                               || App <- rebar_state:all_deps(State)],

            EBinPaths = lists:filter(fun (EbinPath) ->
                                             IsAll orelse (not lists:prefix(SystemLibDir, EbinPath))
                                     end, code:get_path()),
            lists:foreach(fun(EBinPath) ->
                                  DoAdd = case proplists:lookup(filename:absname(EBinPath), DepEbinAndApps) of
                                              none     -> true;
                                              {_, App} -> not lists:member(rebar_app_info:name(App), ExcludeAppNames)
                                          end,
                                  case DoAdd of
                                      true  -> xref:add_directory(Xref, EBinPath);
                                      false -> ok
                                  end
                          end, EBinPaths)
    end.

%% @doc xrefを使って`AppName'が直接依存(使用)しているアプリケーション群を取得する
-spec collect_direct_depending_applications(AppName, rebar_state:t()) -> ordsets:ordset(AppName) when
      AppName :: atom().
collect_direct_depending_applications(AppName, State) ->
    {ok, Calls0} = xref:analyze(?XREF_SERVER, {application_call, AppName}),
    Calls1 = ordsets:from_list(Calls0),
    case rebar_app_info:name(rebar_state:currennt_app(State)) =:= atom_to_binary(AppName, utf8) of
        false -> ordsets:del_element(AppName, Calls1);
        true  ->
            %% ルートアプリケーションは全てのサブアプリケーションを依存に含める
            Includes = rebar_state:project_apps(State),
            ?DEBUG("includes: ~w", [Includes]),
            ordsets:del_element(AppName, ordsets:union(Calls1, Includes))
    end.

-spec rewrite_applications(filename:name(), [atom()]) -> ok | {error, Reason::term()}.
rewrite_applications(AppFile, Applications) ->
    EbinAppFile = rebar_app_utils:app_src_to_app(AppFile),
    {ok, [{application, AppName, AppKeys0}]} = file:consult(EbinAppFile),
    AppKeys1 = lists:keystore(applications, 1, AppKeys0, {applications, Applications}),
    file:write_file(EbinAppFile, io_lib:format("~p.\n", [{application, AppName, AppKeys1}])).
