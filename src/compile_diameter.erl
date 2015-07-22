-module(compile_diameter).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, diameter},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 compile diameter"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, short_desc()},
            {desc, desc()}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Compiling diameter files...", []),
    Dir = rebar_state:dir(State),
    case rebar_app_discover:find_app(Dir, all) of
        false ->
            AllApps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State);
        {true, AppInfo} ->
            AllApps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State) ++ [AppInfo]
    end,
    lists:foreach(fun(App) -> compile(State, App) end, AllApps),
    {ok, State}.


compile(State, AppFile) ->
    % CurrDir = file:get_cwd(),
    % file:set_cwd(rebar_app_info:dir(AppFile)),

    AppDir = rebar_app_info:dir(AppFile),
    io:format("AppDir: ~p~n", [AppDir]),
    DiaDir = filename:join(AppDir, "dia"),
    io:format("DiaDir: ~p~n", [DiaDir]),

    DiaOpts = rebar_state:get(State, dia_opts, []),
    IncludeEbin = proplists:get_value(include, DiaOpts, []),

    DiaPath = filename:join([AppDir, "dia/*.dia"]),
    DiaFiles = filelib:wildcard(DiaPath),

    io:format("DiaFiles: ~p~n", [DiaFiles]),

    code:add_pathsz([filename:join([AppDir, "ebin"]) | filename:join([AppDir, IncludeEbin])]),

    % DiaFirst = case rebar_state:get(State, dia_first_files, []) of
    %     [] ->
    %         [];
    %     CompileFirst ->
    %         CompileFirst
    % end,
    % io:format("DiaFirst: ~p~n", [DiaFirst]),

    FileSequence = case rebar_state:get(State, dia_first_files, []) of
        [] ->
            DiaFiles;
        CompileFirst ->
            CompileFirst ++
            [F || F <- DiaFiles, not lists:member(F, CompileFirst)]
    end,

    io:format("FileSequence: ~p~n", [FileSequence]),

    rebar_base_compiler:run(State,
                            FileSequence,
                            DiaDir,
                            ".dia",
                            filename:join([AppDir, "src"]),
                            ".erl",
                            fun compile_dia/3).
    % file:set_cwd(CurrDir).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%% ===================================================================
%% Internal functions
%% ===================================================================

short_desc() ->
    "Build Diameter (*.dia) sources".

desc() ->
    short_desc() ++ "\n"
       "\n"
       "Valid rebar.config options:~n"
       "  {dia_opts, []} (options from diameter_make:codec/2 supported with~n"
       "                  exception of inherits)~n"
       "  {dia_first_files, []} (files in sequence to compile first)~n".

-spec compile_dia(file:filename(), file:filename(),
                   rebar_config:config()) -> ok.
compile_dia(Source, Target, State) ->
    io:format("Source: ~p~n", [Source]),
    io:format("Target: ~p~n", [Target]),
    ok = filelib:ensure_dir(Target),
    ok = filelib:ensure_dir(filename:join("include", "dummy.hrl")),
    Opts = [{outdir, "src"}] ++ rebar_state:get(State, dia_opts, []),
    case diameter_dict_util:parse({path, Source}, []) of
        {ok, Spec} ->
            FileName = dia_filename(Source, Spec),
            io:format("FileName: ~p~n", [FileName]),
            _ = diameter_codegen:from_dict(FileName, Spec, Opts, erl),
            _ = diameter_codegen:from_dict(FileName, Spec, Opts, hrl),
            HrlFile = filename:join("src", FileName ++ ".hrl"),
            ErlFile = filename:join("src", FileName ++ ".erl"),
            ErlCOpts = [{outdir, "ebin"}] ++
                        rebar_state:get(State, erl_opts, []),
            _ = compile:file(ErlFile, ErlCOpts),
            case filelib:is_regular(HrlFile) of
                true ->
                    ok = rebar_file_utils:mv(HrlFile, "include");
                false ->
                    ok
            end;
        {error, Reason} ->
            rebar_api:error(
                "Compiling ~s failed: ~s~n",
                [Source, diameter_dict_util:format_error(Reason)]
            )
    end.

dia_filename(File, Spec) ->
    case proplists:get_value(name, Spec) of
        undefined ->
            filename:rootname(filename:basename(File));
        Name ->
            Name
    end.
