%% @doc
%% Rebar3 plugin for compiling Diameter protocol dictionary files (.dia).
%%
%% This module provides compilation support for Diameter protocol dictionary files
%% in rebar3 projects. It automatically discovers .dia files in the dia/ directory,
%% resolves dependencies based on @inherits directives, and generates corresponding
%% .erl and .hrl files for use with Erlang's diameter application.
%%
%% == Configuration ==
%%
%% The plugin supports the following rebar.config options:
%% ```
%% {dia_opts, [
%%     {outdir, "custom_src"},           % Output directory for .erl files (default: "src")
%%     {include, ["path/to/deps"]},      % Additional include paths
%%     {recursive, true}                 % Recursively search for .dia files (default: true)
%% ]}.
%%
%% {dia_first_files, [
%%     "base_dictionary.dia"             % Files to compile first
%% ]}.
%%
%% {dia_only_files, [
%%     "specific_dict"                   % Only compile these dictionaries
%% ]}.
%% '''
%%
%% == Usage ==
%%
%% The plugin is typically invoked automatically via provider hooks:
%% ```
%% rebar3 diameter compile
%% '''
%%
%% @author Carlos Eduardo de Paula
%% @since 1.0.0
%% @end
-module(compile_diameter).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Initialize the diameter compile provider.
%%
%% Sets up the rebar3 provider for compiling diameter files. This function
%% registers the provider with the rebar3 system and defines its properties
%% such as dependencies, description, and command-line options.
%%
%% @param State The current rebar3 state
%% @returns {ok, UpdatedState} with the provider registered
%% @end
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

%% @doc Execute the diameter compilation process.
%%
%% This function performs the main compilation work:
%% <ol>
%% <li>Discovers all .dia files in the project</li>
%% <li>Resolves dependencies between dictionaries</li>
%% <li>Compiles files in dependency order</li>
%% <li>Generates .erl and .hrl files</li>
%% </ol>
%%
%% @param State The current rebar3 state
%% @returns {ok, State} on successful compilation
%% @end
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

%% @doc Compile diameter files for a specific application.
%%
%% This function handles the compilation process for a single application:
%% <ul>
%% <li>Sets up directory paths and code paths</li>
%% <li>Discovers .dia files using configurable patterns</li>
%% <li>Determines compilation order based on dependencies</li>
%% <li>Invokes the rebar base compiler with diameter-specific logic</li>
%% </ul>
%%
%% @param State The rebar3 state containing configuration
%% @param AppFile Application info containing directories and options
%% @end
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

%% @doc Format error messages for user display.
%%
%% @param Reason The error term to format
%% @returns Formatted error message as iolist
%% @end
-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Return short description for the provider.
%% @private
short_desc() ->
    "Build Diameter (*.dia) sources".

%% @doc Return detailed description and configuration help.
%% @private
desc() ->
    short_desc() ++
        "\n"
        "\n"
        "Valid rebar.config options:~n"
        "  {dia_opts, []} (options from diameter_make:codec/2 supported with~n"
        "                  exception of inherits)~n"
        "  {dia_first_files, []} (files in sequence to compile first)~n".

%% @doc Compile a single diameter dictionary file.
%%
%% This function handles the compilation of one .dia file:
%% <ol>
%% <li>Parses the diameter dictionary specification</li>
%% <li>Generates .erl and .hrl files using diameter_codegen</li>
%% <li>Compiles the generated .erl file to .beam</li>
%% <li>Loads the compiled module for immediate use</li>
%% </ol>
%%
%% @param Config Rebar configuration options
%% @param Source Path to the source .dia file
%% @param Target Path where the generated .erl file should be placed
%% @param {AppDir, EbinDir} Tuple of application and binary directories
%% @returns ok on success, logs errors and returns error term on failure
%% @end
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

%% @doc Extract the dictionary name from file or spec.
%%
%% Determines the name to use for generated files. Uses the 'name' field
%% from the diameter specification if present, otherwise derives it from
%% the source filename.
%%
%% @param File Path to the source .dia file
%% @param Spec Parsed diameter dictionary specification
%% @returns String name for the dictionary
%% @private
dia_filename(File, Spec) ->
    case proplists:get_value(name, Spec) of
        undefined ->
            filename:rootname(
                filename:basename(File)
            );
        Name ->
            Name
    end.

%% @doc Determine compilation order based on dependencies.
%%
%% This function analyzes @inherits directives in .dia files to build a
%% dependency graph and determine the correct compilation order. Files that
%% are inherited by others must be compiled first.
%%
%% The algorithm:
%% <ol>
%% <li>Parse each .dia file to extract @inherits directives</li>
%% <li>Build a directed graph of dependencies</li>
%% <li>Perform topological sort to determine compilation order</li>
%% <li>Filter based on dia_only_files configuration if specified</li>
%% </ol>
%%
%% @param DiaFiles List of discovered .dia file paths
%% @param _DiaOpts Diameter compilation options (currently unused)
%% @param State Rebar3 state containing configuration
%% @returns {ok, OrderedFileList} with files in dependency order
%% @private
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

%% @doc Add a package and its dependencies to a digraph.
%%
%% Helper function adapted from rebar_digraph for building dependency graphs.
%% Creates vertices and edges in the graph to represent inheritance relationships.
%%
%% Originally taken from rebar_digraph.
%%
%% @param Graph The digraph to modify
%% @param {PkgName, Deps} Package name and list of its dependencies
%% @returns ok
%% @private
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

%% @doc Remove duplicates from a list while preserving order.
%%
%% Efficient deduplication using a map to track seen elements.
%% The first occurrence of each element is kept.
%%
%% @param L Input list that may contain duplicates
%% @returns List with duplicates removed, order preserved
%% @private
uniq(L) ->
    uniq_1(L, #{}).

%% @private Helper for uniq/1
uniq_1([X | Xs], M) ->
    case is_map_key(X, M) of
        true ->
            uniq_1(Xs, M);
        false ->
            [X | uniq_1(Xs, M#{X => true})]
    end;
uniq_1([], _) ->
    [].
