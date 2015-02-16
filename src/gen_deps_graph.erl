%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc アプリケーション群の依存関係をdot形式で出力するためのescript
-module(gen_deps_graph).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(XREF, ?MODULE).
-type dep_type() :: call | use.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([main/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec main([string()]) -> no_return().
main([Type, Depth, RootAppStrs]) ->
    main([Type, Depth, RootAppStrs, ""]);
main([TypeStr, DepthStr, RootAppStrs, ExcludeAppStrs]) ->
    Type = case TypeStr of
               "call" -> call;
               "use"  -> use
           end,
    Depth = list_to_integer(DepthStr),
    RootApps = lists:map(fun erlang:list_to_atom/1, string:tokens(RootAppStrs, ",")),
    ExcludeApps = lists:map(fun erlang:list_to_atom/1, string:tokens(ExcludeAppStrs, ",")),
    ok = init_xref(gb_sets:from_list(ExcludeApps)),
    ok = generate(Type, Depth, RootApps),
    halt(0);
main(_) ->
    show_help().

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec show_help() -> no_return().
show_help() ->
    _ = io:format("Usage: ~s call|use DEPTH ROOT_APPLICATION[,ROOT_APPLICATION]* [EXCLUDE_APPLICATION[,EXCLUDE_APPLICATION]*]\n", [?MODULE]),
    halt(1).

-spec init_xref(gb_sets:set(atom())) -> ok.
init_xref(ExcludApps) ->
    {ok, _} = xref:start(?XREF, [{xref_mode, modules}]),
    ok = lists:foreach(
           fun (EbinPath) ->
                   case filelib:wildcard(filename:join(EbinPath, "*.app")) of
                       [AppResourceFile] ->
                           {ok, [{application, App, _}]} = file:consult(AppResourceFile),
                           case gb_sets:is_member(App, ExcludApps) of
                               true  -> ok;
                               false -> xref:add_application(?XREF, filename:dirname(EbinPath), [{name, App}]) % may fail
                           end;
                       _ -> ok
                   end
           end,
           code:get_path()),
    ok.

-spec generate(dep_type(), non_neg_integer(), [atom()]) -> ok.
generate(Type, Depth, Apps) ->
    _ = io:format("digraph ~s {\n", [hd(Apps)]),
    _ = lists:foldl(fun (App, Done) -> generate(Type, Depth, App, Done) end, gb_sets:empty(), Apps),
    _ = io:format("}\n"),
    ok.

-spec generate(dep_type(), non_neg_integer(), atom(), gb_sets:set(atom())) -> gb_sets:set(atom()).
generate(_Type, Depth, _App, Done) when Depth =< 0 ->
    Done;
generate(Type, Depth, App, Done) ->
    case gb_sets:is_member(App, Done) of
        true  -> Done;
        false ->
            {ok, Deps0} =
                case Type of
                    call -> xref:analyze(?XREF, {application_call, App});
                    use  -> xref:analyze(?XREF, {application_use, App})
                end,
            Deps = lists:delete(App, Deps0),
            lists:foldl(
              fun (DepApp, AccDone) ->
                      _ = case Type of
                              call -> io:format(" ~s -> ~s;\n", [App, DepApp]);
                              use  -> io:format(" ~s -> ~s;\n", [DepApp, App])
                          end,
                      generate(Type, Depth - 1, DepApp, AccDone)
              end,
              gb_sets:add_element(App, Done),
              Deps)
    end.
