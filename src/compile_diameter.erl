-module(compile_diameter).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        % The 'user friendly' name of the task
        providers:create([
            {name, ?PROVIDER},
            % The module implementation of the task
            {module, ?MODULE},
            {namespace, diameter},
            {bare,
                % The task can be run by the user, always true
                true},
            % The list of dependencies
            {deps, ?DEPS},
            % How to use the plugin
            {example, "rebar3 compile diameter"},
            % list of options understood by the plugin
            {opts, []},
            {short_desc, short_desc()},
            {desc, desc()}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    rebar_api:info("Compiling diameter files...", []),
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    lists:foreach(fun(App) -> compile(State, App) end, Apps),
    {ok, State}.

compile(State, AppFile) ->
    Opts = rebar_app_info:opts(AppFile),
    AppDir = rebar_app_info:dir(AppFile),
    DiaDir = filename:join(AppDir, "dia"),
    SrcDir = filename:join(AppDir, "src"),
    EbinDir = rebar_app_info:ebin_dir(AppFile),
    rebar_api:debug("AppDir: ~p~n", [AppDir]),
    rebar_api:debug("EbinDir: ~p~n", [AppDir]),

    DiaOpts = rebar_state:get(State, dia_opts, []),
    IncludeEbin = proplists:get_value(include, DiaOpts, []),

    code:add_pathsz([EbinDir | filename:join([AppDir, IncludeEbin])]),

    DiaFirst =
        case rebar_state:get(State, dia_first_files, []) of
            [] ->
                [];
            CompileFirst ->
                [filename:join(DiaDir, filename:basename(F)) || F <- CompileFirst]
        end,

    DiaExtRe = "^(?!\\._).*\\.dia$",
    Recursive = proplists:get_value(recursive, DiaOpts, true),

    %% Find all possible source files
    DiaFiles = rebar_utils:find_files(DiaDir, DiaExtRe, Recursive),
    rebar_api:debug("Diameter files: ~p~n", [DiaFiles]),

    case compile_order(DiaFiles, DiaOpts, State) of
        {error, Reason} ->
            rebar_api:error("DIAMETER error: ~p~n", [Reason]);
        {ok, Order} ->
            rebar_api:debug("Diameter Order: ~p~n", [Order]),

            CompileFun = fun(Source, Target, C) ->
                case lists:member(Source, Order) of
                    true -> compile_dia(C, Source, Target, {AppDir, EbinDir});
                    false -> ok
                end
            end,

            rebar_base_compiler:run(
                Opts,
                DiaFirst ++ Order,
                DiaDir,
                ".dia",
                SrcDir,
                ".erl",
                CompileFun
            )
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

short_desc() ->
    "Build Diameter (*.dia) sources".

desc() ->
    short_desc() ++
        "\n"
        "\n"
        "Valid rebar.config options:~n"
        "  {dia_opts, []} (options from diameter_make:codec/2 supported with~n"
        "                  exception of inherits)~n"
        "  {dia_first_files, []} (files in sequence to compile first)~n".

-spec compile_dia(
    rebar_config:config(),
    file:filename(),
    file:filename(),
    rebar_config:config()
) ->
    ok.
compile_dia(Config, Source, Target, {AppDir, EbinDir}) ->
    rebar_api:debug("Source diameter file: ~p~n", [Source]),
    rebar_api:debug("Target diameter file: ~p~n", [Target]),

    ok = filelib:ensure_dir(Target),
    ok =
        filelib:ensure_dir(
            filename:join([AppDir, "include", "dummy.hrl"])
        ),
    ok =
        filelib:ensure_dir(
            filename:join([EbinDir, "dummy.beam"])
        ),

    OutDir = filename:join(AppDir, "src"),
    IncludeOutDir = filename:join(AppDir, "include"),

    DiaOpts = rebar_opts:get(Config, dia_opts, []),
    Opts = [{outdir, OutDir}] ++ DiaOpts,
    IncludeOpts = [{outdir, IncludeOutDir}] ++ DiaOpts,
    case diameter_dict_util:parse({path, Source}, DiaOpts) of
        {ok, Spec} ->
            FileName = dia_filename(Source, Spec),
            _ = diameter_codegen:from_dict(FileName, Spec, Opts, erl),
            _ = diameter_codegen:from_dict(FileName, Spec, IncludeOpts, hrl),
            ErlCOpts = [{outdir, EbinDir}, return_errors] ++ rebar_opts:get(Config, erl_opts, []),
            TargetName = proplists:get_value(name, Spec, filename:basename(Target, ".erl")),
            RealTarget = filename:join([filename:dirname(Target), [TargetName, ".erl"]]),
            case compile:file(RealTarget, ErlCOpts) of
                {ok, Module} ->
                    code:purge(Module),
                    case code:load_abs(EbinDir ++ "/" ++ atom_to_list(Module)) of
                        {error, LoadError} ->
                            rebar_api:error(
                                "Can't load DIAMTER dictionary ~p, error '~p'",
                                [FileName, LoadError]
                            );
                        {module, _} ->
                            ok
                    end;
                Other ->
                    rebar_api:error(
                        "Can't compile DIAMTER dictionary ~p, error '~p'",
                        [FileName, Other]
                    ),
                    Other
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
            filename:rootname(
                filename:basename(File)
            );
        Name ->
            Name
    end.

compile_order(DiaFiles, _, State) ->
    Graph = digraph:new(),

    DiaMods =
        lists:foldl(
            fun(F, M) ->
                Dict =
                    filename:rootname(
                        filename:basename(F)
                    ),
                M#{Dict => F}
            end,
            #{},
            DiaFiles
        ),
    maps:map(
        fun(Dict, F) ->
            try
                {ok, Bin} = file:read_file(F),
                Inherits0 = binary:split(Bin, [<<$\n>>, <<$\r>>], [global, trim_all]),
                Inherits1 =
                    [binary:split(I, [<<" ">>, <<$\t>>], [global, trim_all]) || I <- Inherits0],
                Inherits = [I || [<<"@inherits">>, I | _] <- Inherits1],
                rebar_api:debug("Inherits for ~p: ~p~n", [Dict, Inherits]),
                add(Graph, {Dict, Inherits})
            catch
                _:_ ->
                    ok
            end
        end,
        DiaMods
    ),

    AllDicts = lists:map(fun(F) -> filename:rootname(filename:basename(F)) end, DiaFiles),

    DiaOnlyFiles = rebar_state:get(State, dia_only_files, AllDicts),
    DiaOnlyFiles1 = lists:map(
        fun(File) ->
            case File of
                F when is_list(F) -> F;
                F when is_atom(F) -> atom_to_list(F);
                F when is_binary(F) -> binary_to_list(F)
            end
        end,
        DiaOnlyFiles
    ),

    FilteredDicts = lists:filter(fun(F) -> lists:member(F, DiaOnlyFiles1) end, AllDicts),

    Order = lists:flatmap(
        fun(F) ->
            Reachable = digraph_utils:reachable([F], Graph),
            SubGraph = digraph_utils:subgraph(Graph, Reachable),
            TopSorted = digraph_utils:topsort(SubGraph),

            true = digraph:delete(SubGraph),

            lists:foldl(
                fun(X, Acc) ->
                    case maps:get(X, DiaMods, undefined) of
                        undefined -> Acc;
                        File -> [File | Acc]
                    end
                end,
                [],
                TopSorted
            )
        end,
        FilteredDicts
    ),

    true = digraph:delete(Graph),

    {ok, uniq(Order)}.

%% taken from rebar_digraph:
%% @private Add a package and its dependencies to an existing digraph
-spec add(digraph:graph(), {PkgName, [Dep]}) -> ok when
    PkgName :: binary(),
    Dep :: {Name, term()} | Name,
    Name :: atom() | iodata().
add(Graph, {PkgName, Deps}) ->
    case digraph:vertex(Graph, PkgName) of
        false ->
            V = digraph:add_vertex(Graph, PkgName);
        {V, []} ->
            V
    end,

    lists:foreach(
        fun(Name1) ->
            Name =
                case Name1 of
                    N when is_atom(N) ->
                        atom_to_list(Name1);
                    {N, _} when is_list(N) ->
                        N;
                    N when is_list(N) ->
                        N;
                    N when is_binary(N) ->
                        binary_to_list(N)
                end,
            V3 =
                case digraph:vertex(Graph, Name) of
                    false ->
                        digraph:add_vertex(Graph, Name);
                    {V2, []} ->
                        V2
                end,
            digraph:add_edge(Graph, V, V3)
        end,
        Deps
    ).

uniq(L) ->
    uniq_1(L, #{}).

uniq_1([X | Xs], M) ->
    case is_map_key(X, M) of
        true ->
            uniq_1(Xs, M);
        false ->
            [X | uniq_1(Xs, M#{X => true})]
    end;
uniq_1([], _) ->
    [].
