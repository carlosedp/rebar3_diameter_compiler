%% -------------------------------------------------------------------
%%
%% rebar3 diameter cleaner, in the manner of rebar2.
%%
%% -------------------------------------------------------------------
-module(clean_diameter).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},             %% The 'user friendly' name of the task
                                 {module, ?MODULE},             %% The module implementation of the task
                                 {bare, true},                  %% The task can be run by the user, always true
                                 {deps, ?DEPS},                 %% The list of dependencies
                                 {example, "rebar diameter clean"},   %% How to use the plugin
                                 {opts, []},                    %% list of options understood by the plugin
                                 {short_desc, "Clean compiled diameter files."},
                                 {desc, ""},
                                 {namespace, diameter}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Cleaning compiled diameter files...", []),
    Dir = rebar_state:dir(State),
    io:format("Dir: ~p~n", [Dir]),

    case rebar_app_discover:find_app(Dir, all) of
        false ->
            io:format("False~n"),
            AllApps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
            io:format("AllApps: ~p~n", [AllApps]),
            lists:foreach(fun(A) -> clean(A, State) end, AllApps);
        {true, AppInfo} ->
            io:format("True: ~p~n", [AppInfo]),
            io:format("AppInfo: ~p~n", [AppInfo]),
            clean(State, AppInfo)
    end.

    % {true, AppInfo} = rebar_app_discover:find_app(Dir, all),

    % AllApps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
    % io:format("AllApps: ~p~n", [AllApps]),

    % case rebar_app_utils:find(rebar_app_info:name(AppInfo), AllApps) of
    %     {ok, AppInfo1} ->
    %         %% Use the existing app info instead of newly created one

    %         io:format("Case 1: ~p~n", [AppInfo1]),
    %         clean(State, AppInfo1);
    %     _ ->
    %         io:format("Case 2: ~p~n", [AppInfo]),
    %         clean(State, AppInfo)
    % end.

    % lists:foreach(fun(A) -> clean(State, A) end, rebar_state:project_apps(State)),
    % {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

clean(AppInfo, State) ->
    Dir = rebar_state:dir(State),
    io:format("Clean-Dir: ~p~n", [Dir]),
    io:format("Clean-CWD-before: ~p~n", [file:get_cwd()]),

    file:set_cwd(rebar_app_info:dir(AppInfo)),
    io:format("Clean-CWD-after: ~p~n", [file:get_cwd()]),
    DiaOpts = rebar_state:get(State, dia_opts, []),
    IncludeEbin = proplists:get_value(include, DiaOpts, []),
    code:add_pathsz(["ebin" | IncludeEbin]),
    GeneratedFiles = dia_generated_files("dia", "src", "include"),
    ok = rebar_file_utils:delete_each(GeneratedFiles),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

dia_generated_files(DiaDir, SrcDir, IncDir) ->
    F = fun(File, Acc) ->
            case catch diameter_dict_util:parse({path, File}, []) of
                {ok, Spec} ->
                    FileName = dia_filename(File, Spec),
                    [
                        filename:join([IncDir, FileName ++ ".hrl"]) |
                        filelib:wildcard(
                            filename:join([SrcDir, FileName ++ ".*"])
                        )
                    ] ++ Acc;
                _ ->
                    Acc
            end
    end,
    lists:foldl(F, [], filelib:wildcard(filename:join([DiaDir, "*.dia"]))).

dia_filename(File, Spec) ->
    case proplists:get_value(name, Spec) of
        undefined ->
            filename:rootname(filename:basename(File));
        Name ->
            Name
    end.
