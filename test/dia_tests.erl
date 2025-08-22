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
    {ok, CurrentDir} = file:get_cwd(),
    Repo = find_repo_root(CurrentDir),
    Branch = setup_git_branch(),
    Test_target = test_target(Repo),
    setup_delete(Test_target),
    setup_create(Test_target, Repo, Branch, Only),
    setup_test_run(test_run_directory(Repo)).

%% Find the repository root by looking for key files
find_repo_root(Dir) ->
    % Look for rebar.config and src directory to identify repo root
    RebarConfig = filename:join(Dir, "rebar.config"),
    SrcDir = filename:join(Dir, "src"),
    TestDir = filename:join(Dir, "test"),

    case {filelib:is_regular(RebarConfig), filelib:is_dir(SrcDir), filelib:is_dir(TestDir)} of
        {true, true, true} ->
            % Found all three indicators of repo root
            Dir;
        _ ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                true ->
                    % Reached filesystem root, something is wrong
                    % Try a more aggressive search starting from a known CI path pattern
                    case string:str(Dir, "rebar3_diameter_compiler") of
                        % fallback to current dir
                        0 ->
                            Dir;
                        Pos ->
                            % Extract everything up to and including rebar3_diameter_compiler
                            BasePos = Pos + length("rebar3_diameter_compiler") - 1,
                            Candidate = string:substr(Dir, 1, BasePos),
                            case
                                {
                                    filelib:is_regular(filename:join(Candidate, "rebar.config")),
                                    filelib:is_dir(filename:join(Candidate, "src")),
                                    filelib:is_dir(filename:join(Candidate, "test"))
                                }
                            of
                                {true, true, true} -> Candidate;
                                % fallback
                                _ -> Dir
                            end
                    end;
                false ->
                    find_repo_root(Parent)
            end
    end.

setup_create(Test_target, Repo, Branch, Only) ->
    [ok = setup_create_dia(Test_target, X) || X <- diameter_files()],
    ok = setup_create_src(Test_target),
    ok = setup_create_rebar_config(Test_target, Repo, Branch, Only).

setup_create_dia(Test_target, BaseName) ->
    Diameter_file = BaseName ++ ".dia",
    File = filename:join([Test_target, "dia", Diameter_file]),
    ok = filelib:ensure_dir(File),
    {ok, CurrentDir} = file:get_cwd(),
    Repo = find_repo_root(CurrentDir),
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
    GithubHeadRef = os:getenv("GITHUB_HEAD_REF"),
    GithubRefName = os:getenv("GITHUB_REF_NAME"),
    case GithubHeadRef of
        false ->
            case GithubRefName of
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
        [] ->
            % Empty GITHUB_HEAD_REF, fallback to GITHUB_REF_NAME
            case GithubRefName of
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
    {ok, CurrentDir} = file:get_cwd(),
    Repo = find_repo_root(CurrentDir),
    Test_target = test_target(Repo),
    ok = file:set_cwd(Test_target),
    %	Result = os:cmd( "DIAGNOSTIC=1 rebar3 eunit" ),
    %	?debugMsg( Result ),
    % Try to use system rebar3 first (for CI), then local rebar3
    Rebar3Cmd =
        case os:find_executable("rebar3") of
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
            {ok, CurrentDir} = file:get_cwd(),
            Repo = find_repo_root(CurrentDir),
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
    {ok, CurrentDir} = file:get_cwd(),
    Repo = find_repo_root(CurrentDir),
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
    % Filter out OTP version differences like -moduledoc(false)
    FilteredF1 = filter_otp_differences(F1),
    FilteredF2 = filter_otp_differences(F2),
    ?assertEqual(FilteredF1, FilteredF2).

%% Filter out OTP version differences in generated code
filter_otp_differences(Lines) ->
    CleanedLines = [Line || Line <- Lines, not is_otp_version_difference(Line)],
    % Normalize empty lines after module declaration
    normalize_module_section(CleanedLines).

is_otp_version_difference(<<"-moduledoc(false).">>) -> true;
is_otp_version_difference(_) -> false.

%% Normalize empty lines in the module section
normalize_module_section(Lines) ->
    normalize_module_section(Lines, [], false, false).

normalize_module_section([], Acc, _AfterModule, _HasEmptyAfterModule) ->
    lists:reverse(Acc);
normalize_module_section([Line | Rest], Acc, false, false) ->
    case Line of
        <<"-module(", _/binary>> ->
            normalize_module_section(Rest, [Line | Acc], true, false);
        _ ->
            normalize_module_section(Rest, [Line | Acc], false, false)
    end;
normalize_module_section([<<>> | Rest], Acc, true, false) ->
    % First empty line after module - keep it and mark that we've seen one
    normalize_module_section(Rest, [<<>> | Acc], true, true);
normalize_module_section([<<>> | Rest], Acc, true, true) ->
    % Skip additional empty lines after module section
    normalize_module_section(Rest, Acc, true, true);
normalize_module_section([Line | Rest], Acc, true, _HasEmpty) ->
    % Non-empty line after module section - continue normally
    normalize_module_section(Rest, [Line | Acc], false, false).
