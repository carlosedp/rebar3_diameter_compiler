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
    AppDir = rebar_app_info:dir(AppFile),
    DiaDir = filename:join(AppDir, "dia"),
    SrcDir = filename:join(AppDir, "src"),
    EbinDir = rebar_app_info:ebin_dir(AppFile),
    rebar_api:debug("AppDir: ~p~n", [AppDir]),
    rebar_api:debug("EbinDir: ~p~n", [AppDir]),

    DiaOpts = rebar_state:get(State, dia_opts, []),
    IncludeEbin = proplists:get_value(include, DiaOpts, []),

    code:add_pathsz([EbinDir | filename:join([AppDir, IncludeEbin])]),

    DiaFirst = case rebar_state:get(State, dia_first_files, []) of
        [] ->
            [];
        CompileFirst ->
            [filename:join(DiaDir, filename:basename(F)) || F <- CompileFirst]
    end,
    rebar_api:debug("Diameter first files: ~p~n", [DiaFirst]),

    rebar_base_compiler:run({State, AppDir, EbinDir},
                            DiaFirst,
                            DiaDir,
                            ".dia",
                            SrcDir,
                            ".erl",
                            fun compile_dia/3).

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
compile_dia(Source, Target, {State, AppDir, EbinDir}) ->
    rebar_api:debug("Source diameter file: ~p~n", [Source]),
    rebar_api:debug("Target diameter file: ~p~n", [Target]),

    ok = filelib:ensure_dir(Target),
    ok = filelib:ensure_dir(filename:join([AppDir, "include", "dummy.hrl"])),
    ok = filelib:ensure_dir(filename:join([EbinDir, "dummy.beam"])),

    OutDir = filename:join(AppDir, "src"),
    IncludeOutDir = filename:join(AppDir, "include"),

    Opts = [{outdir, OutDir}] ++ rebar_state:get(State, dia_opts, []),
    IncludeOpts = [{outdir, IncludeOutDir}] ++ rebar_state:get(State, dia_opts, []),
    case diameter_dict_util:parse({path, Source}, rebar_state:get(State, dia_opts, [])) of
        {ok, Spec} ->
            FileName = dia_filename(Source, Spec),
            _ = diameter_codegen:from_dict(FileName, Spec, Opts, erl),
            _ = diameter_codegen:from_dict(FileName, Spec, IncludeOpts, hrl),
            ErlCOpts = [{outdir, EbinDir}, return_errors] ++
                        rebar_state:get(State, erl_opts, []),
            case compile:file(Target, ErlCOpts) of
                {ok, _} -> ok;
                {error, Reason} ->
                    rebar_api:error(
                      "Compiling ~s failed: ~s~n",
                      [Source, diameter_dict_util:format_error(Reason)]
                     )
            end;
        {error, Reason, _} ->
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
