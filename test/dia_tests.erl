-module(dia_tests).

-include_lib("eunit/include/eunit.hrl").

compile_only_test_() ->
    {setup, fun() -> setup("baz") end,
        {timeout, 30, [
            fun() -> test_compile(["foo", "bar", "baz"], [diameter_basename()]) end
        ]}}.

compile_test_() ->
    {setup, fun() -> setup() end, {timeout, 30, [fun() -> test_compile(diameter_files(), []) end]}}.

compare_test() ->
    compare_files().

%% Internal

diameter_basename() ->
    "diameter_3gpp_base".

diameter_files() ->
    [
        diameter_basename(),
        "foo",
        "bar",
        "baz"
    ].

setup() ->
    setup(undefined).

%% Setup test/compile directory in _build/test/lib/rebar3_diameter_compiler
%% In this directory ''rebar3 diameter compile'' will run.
%% Replace rebar.config from repo (the one with dialyzer, etc) to save time.
%% Getting rebar3_diameter_compiler dependencies to this directory will take some time.
%% More than 5 seconds, less than 30 seconds on my machine.
setup(Only) ->
    {ok, Repo} = file:get_cwd(),
    Branch = setup_git_branch(),
    Test_target = test_target(Repo),
    setup_delete(Test_target),
    setup_create(Test_target, Repo, Branch, Only),
    setup_test_run(test_run_directory(Repo)).

setup_create(Test_target, Repo, Branch, Only) ->
    [ok = setup_create_dia(Test_target, X) || X <- diameter_files()],
    ok = setup_create_src(Test_target),
    ok = setup_create_rebar_config(Test_target, Repo, Branch, Only).

setup_create_dia(Test_target, BaseName) ->
    Diameter_file = BaseName ++ ".dia",
    File = filename:join([Test_target, "dia", Diameter_file]),
    ok = filelib:ensure_dir(File),
    {ok, Repo} = file:get_cwd(),
    SourceFile = filename:join([Repo, "test", Diameter_file]),
    case filelib:is_regular(SourceFile) of
        true ->
            {ok, _} = file:copy(SourceFile, File),
            ok;
        false ->
            error({source_file_not_found, SourceFile, Repo})
    end.

setup_create_src(Test_target) ->
    App = "compile",
    File = filename:join([Test_target, "src", App ++ ".app.src"]),
    ok = filelib:ensure_dir(File),
    file:write_file(File, setup_create_src_content(App)).

setup_create_src_content(App) ->
    "\n"
    "{application, " ++
        App ++
        ", [\n"
        "\t{description, \"" ++
        App ++
        "\"},\n"
        "\t{vsn, \"1.0\"},\n"
        "\t{applications, [kernel,stdlib]}\n"
        "]}.\n".

setup_create_rebar_config(Test_target, Repo, Branch, Only) ->
    File = filename:join(Test_target, "rebar.config"),
    ok = filelib:ensure_dir(File),
    file:write_file(File, setup_rebar_config_content(Repo, Branch, Only)).

setup_delete(Directory) ->
    Paths =
        filelib:wildcard(
            filename:join(Directory, "*")
        ),
    {Directories, Files} = lists:partition(fun filelib:is_dir/1, Paths),
    [file:delete(X) || X <- Files],
    [setup_delete(X) || X <- Directories],
    file:del_dir(Directory).

setup_git_branch() ->
    % In CI environments, we might be in detached HEAD state
    % Try multiple GitHub Actions environment variables
    case os:getenv("GITHUB_HEAD_REF") of
        false ->
            case os:getenv("GITHUB_REF_NAME") of
                false ->
                    % No GitHub environment, try git command
                    Branch = string:trim(os:cmd("git branch --show-current")),
                    case Branch of
                        "" ->
                            % Fallback to 'master' if we can't determine branch
                            "master";
                        _ ->
                            Branch
                    end;
                GithubRefName ->
                    GithubRefName
            end;
        GithubHeadRef ->
            % This is set for pull requests
            GithubHeadRef
    end.

setup_rebar_config_content(Repo, Branch, undefined) ->
    "\n"
    "{plugins, [\n"
    "\t{rebar3_diameter_compiler, {git, \"file://" ++
        Repo ++
        "\", {branch, \"" ++
        Branch ++
        "\"}}}\n"
        "]}.\n"
        "{provider_hooks, [\n"
        "\t{pre, [\n"
        "\t	{clean, {diameter, clean}},\n"
        "\t	{compile, {diameter, compile}}\n"
        "\t]}\n"
        "]}.\n";
setup_rebar_config_content(Repo, Branch, Only) ->
    "\n"
    "{plugins, [\n"
    "\t{rebar3_diameter_compiler, {git, \"file://" ++
        Repo ++
        "\", {branch, \"" ++
        Branch ++
        "\"}}}\n"
        "]}.\n"
        "{provider_hooks, [\n"
        "\t{pre, [\n"
        "\t	{clean, {diameter, clean}},\n"
        "\t	{compile, {diameter, compile}}\n"
        "\t]}\n"
        "]}.\n"
        "{dia_only_files, [" ++ Only ++ "]}.\n".

%% Simple rebar.config when running tests.
setup_test_run(Directory) ->
    file:write_file(
        filename:join(Directory, "rebar.config"), "{erl_opts, [debug_info]}.\n{deps, []}.\n"
    ).

test_compile(CompiledFiles, SkippedFiles) ->
    {ok, Repo} = file:get_cwd(),
    Test_target = test_target(Repo),
    ok = file:set_cwd(Test_target),
    %	Result = os:cmd( "DIAGNOSTIC=1 rebar3 eunit" ),
    %	?debugMsg( Result ),
    % Try to use system rebar3 first (for CI), then local rebar3
    Rebar3Cmd = case os:find_executable("rebar3") of
        false ->
            % No system rebar3, use local one
            filename:join(Repo, "rebar3");
        SystemRebar3 ->
            SystemRebar3
    end,
    ?assertCmd(Rebar3Cmd ++ " diameter compile"),
    [
        ?assert(filelib:is_regular(filename:join("include", File ++ ".hrl")))
     || File <- CompiledFiles
    ],
    [
        ?assert(filelib:is_regular(filename:join("src", File ++ ".erl")))
     || File <- CompiledFiles
    ],
    [
        ?assertNot(filelib:is_regular(filename:join("include", File ++ ".hrl")))
     || File <- SkippedFiles
    ],
    [
        ?assertNot(filelib:is_regular(filename:join("src", File ++ ".erl")))
     || File <- SkippedFiles
    ],
    file:set_cwd(Repo).

test_run_directory(Repo) ->
    filename:join([
        Repo,
        "_build",
        "test",
        "lib",
        "rebar3_diameter_compiler"
    ]).

test_target(Repo) ->
    filename:join([
        test_run_directory(Repo),
        "test",
        "compile"
    ]).

compare_files() ->
    Golden = os:getenv("GOLDEN_RUN"),
    golden_run(Golden, "erl", "src"),
    golden_run(Golden, "hrl", "include"),

    compare_files("erl", "src"),
    compare_files("hrl", "include").

golden_run(Val, Ext, Dir) ->
    case Val of
        false ->
            true;
        _ ->
            {ok, Repo} = file:get_cwd(),
            Test_target = test_target(Repo),
            Src = filename:join([Test_target, Dir, diameter_basename() ++ "." ++ Ext]),
            Dst = filename:join([
                Repo,
                "test/expected/" ++ Dir,
                diameter_basename() ++ "." ++ Ext ++ "-expected"
            ]),
            ?debugMsg("Copying " ++ Src ++ " file to " ++ Dst),
            file:copy(Src, Dst)
    end.

compare_files(Ext, Dir) ->
    {ok, Repo} = file:get_cwd(),
    Test_target = test_target(Repo),
    Generated = filename:join([Test_target, Dir, diameter_basename() ++ "." ++ Ext]),
    ok = filelib:ensure_dir(Generated),
    {ok, Gen} = file:read_file(Generated),
    F1 = binary:split(Gen, <<"\n">>, [global]),
    Expected =
        filename:join([
            Repo,
            "test/expected/" ++ Dir,
            diameter_basename() ++ "." ++ Ext ++ "-expected"
        ]),
    ok = filelib:ensure_dir(Expected),
    {ok, Exp} = file:read_file(Expected),
    F2 = binary:split(Exp, <<"\n">>, [global]),
    ?assertEqual(F1, F2).
