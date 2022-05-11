-module(dia_tests).

-include_lib("eunit/include/eunit.hrl").

compile_test_() ->
    {setup, fun() -> setup() end, [fun() -> test_compile() end]}.

%% Internal

diameter_basename() ->
    "diameter_3gpp_base".

setup() ->
    {ok, Repo} = file:get_cwd(),
    Branch = setup_git_branch(),
    Test_target = test_target(Repo),
    setup_delete(Test_target),
    setup_create(Test_target, Repo, Branch).

setup_create(Test_target, Repo, Branch) ->
    ok = setup_create_dia(Test_target),
    ok = setup_create_src(Test_target),
    ok = setup_create_rebar_config(Test_target, Repo, Branch).

setup_create_dia(Test_target) ->
    Diameter_file = diameter_basename() ++ ".dia",
    File = filename:join([Test_target, "dia", Diameter_file]),
    ok = filelib:ensure_dir(File),
    {ok, _} =
        file:copy(
            filename:join("test", Diameter_file), File),
    ok.

setup_create_src(Test_target) ->
    App = "compile",
    File = filename:join([Test_target, "src", App ++ ".app.src"]),
    ok = filelib:ensure_dir(File),
    file:write_file(File, setup_create_src_content(App)).

setup_create_src_content(App) ->
    "
{application, "
    ++ App
    ++ ", [
\t{description, \""
    ++ App
    ++ "\"},
\t{vsn, \"1.0\"},
\t{applications, [kernel,stdlib]}
]}.
".

setup_create_rebar_config(Test_target, Repo, Branch) ->
    File = filename:join(Test_target, "rebar.config"),
    ok = filelib:ensure_dir(File),
    file:write_file(File, setup_rebar_config_content(Repo, Branch)).

setup_delete(Directory) ->
    Paths =
        filelib:wildcard(
            filename:join(Directory, "*")),
    {Directories, Files} = lists:partition(fun filelib:is_dir/1, Paths),
    [file:delete(X) || X <- Files],
    [setup_delete(X) || X <- Directories],
    file:del_dir(Directory).

setup_git_branch() ->
    string:trim(
        os:cmd("git branch --show-current")).

setup_rebar_config_content(Repo, Branch) ->
    "
{plugins, [
\t{rebar3_diameter_compiler, {git, \"file://"
    ++ Repo
    ++ "\", {branch, \""
    ++ Branch
    ++ "\"}}}
]}.
{provider_hooks, [
\t{pre, [
\t	{clean, {diameter, clean}},
\t	{compile, {diameter, compile}}
\t]}
]}.
".

test_compile() ->
    {ok, Repo} = file:get_cwd(),
    Test_target = test_target(Repo),
    ok = file:set_cwd(Test_target),
    %	Result = os:cmd( "DIAGNOSTIC=1 rebar3 eunit" ),
    %	?debugMsg( Result ),
    ?assertCmd("rebar3 eunit"),
    true =
        filelib:is_regular(
            filename:join("include", diameter_basename() ++ ".hrl")),
    true =
        filelib:is_regular(
            filename:join("src", diameter_basename() ++ ".erl")),
    file:set_cwd(Repo).

test_target(Repo) ->
    filename:join([Repo,
                   "_build",
                   "test",
                   "lib",
                   "rebar3_diameter_compiler",
                   "test",
                   "compile"]).
