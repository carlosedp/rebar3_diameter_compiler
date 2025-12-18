%% @doc
%% Unit tests for rebar3_diameter_compiler plugin modules.
%%
%% This module provides unit-level testing of individual functions and
%% edge cases that complement the integration tests in dia_tests.erl.
%% It focuses on testing:
%%
%% <ul>
%% <li>Error handling and edge cases</li>
%% <li>Configuration option parsing</li>
%% <li>Dependency resolution logic</li>
%% <li>File naming and path handling</li>
%% <li>Provider initialization</li>
%% </ul>
%%
%% @author Carlos Eduardo de Paula
%% @since 1.0.0
%% @end
-module(unit_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Compile Module Tests
%%%===================================================================

%% @doc Test provider initialization.
%%
%% Verifies that the compile provider can be initialized and registers
%% correctly with rebar3.
init_compile_provider_test() ->
    State = rebar_state:new(),
    {ok, State1} = compile_diameter:init(State),
    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0).

%% @doc Test format_error function.
%%
%% Ensures error messages are properly formatted for display.
format_error_test() ->
    Error = compile_diameter:format_error(test_error),
    ?assert(is_list(Error)),
    ?assert(length(Error) > 0).

%%%===================================================================
%%% Clean Module Tests
%%%===================================================================

%% @doc Test clean provider initialization.
%%
%% Verifies that the clean provider can be initialized and registers
%% correctly with rebar3.
init_clean_provider_test() ->
    State = rebar_state:new(),
    {ok, State1} = clean_diameter:init(State),
    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0).

%% @doc Test clean format_error function.
%%
%% Ensures error messages are properly formatted for display.
clean_format_error_test() ->
    Error = clean_diameter:format_error(clean_error),
    ?assert(is_list(Error)),
    ?assert(length(Error) > 0).

%%%===================================================================
%%% Main Plugin Module Tests
%%%===================================================================

%% @doc Test main plugin initialization.
%%
%% Verifies that the plugin correctly initializes both compile and clean providers.
init_plugin_test() ->
    State = rebar_state:new(),
    {ok, State1} = rebar3_diameter_compiler:init(State),
    Providers = rebar_state:providers(State1),
    % Should have both compile and clean providers
    ?assert(length(Providers) >= 2).

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

%% @doc Test dia_opts configuration parsing.
%%
%% These tests verify that various configuration options are properly
%% handled without actual compilation.
dia_opts_test_() ->
    [
        {"Empty dia_opts", fun() ->
            Opts = [],
            OutDir = proplists:get_value(outdir, Opts, "src"),
            ?assertEqual("src", OutDir)
        end},
        {"Custom outdir", fun() ->
            Opts = [{outdir, "custom_src"}],
            OutDir = proplists:get_value(outdir, Opts, "src"),
            ?assertEqual("custom_src", OutDir)
        end},
        {"Include paths", fun() ->
            Opts = [{include, ["path1", "path2"]}],
            Includes = proplists:get_value(include, Opts, []),
            ?assertEqual(["path1", "path2"], Includes)
        end},
        {"Recursive flag", fun() ->
            Opts = [{recursive, false}],
            Recursive = proplists:get_value(recursive, Opts, true),
            ?assertEqual(false, Recursive)
        end}
    ].

%%%===================================================================
%%% Path and Filename Tests
%%%===================================================================

%% @doc Test filename handling edge cases.
filename_edge_cases_test_() ->
    [
        {"Basename extraction", fun() ->
            Path = "/some/path/to/file.dia",
            Base = filename:basename(Path),
            ?assertEqual("file.dia", Base)
        end},
        {"Rootname extraction", fun() ->
            Path = "/some/path/to/file.dia",
            Root = filename:rootname(filename:basename(Path)),
            ?assertEqual("file", Root)
        end},
        {"File with multiple dots", fun() ->
            Path = "/path/file.test.dia",
            Root = filename:rootname(filename:basename(Path)),
            ?assertEqual("file.test", Root)
        end},
        {"Empty path handling", fun() ->
            Path = "",
            Base = filename:basename(Path),
            % Empty path returns empty list or "."
            ?assert(Base =:= "." orelse Base =:= "")
        end}
    ].

%%%===================================================================
%%% Dependency Graph Tests
%%%===================================================================

%% @doc Test simple dependency graph.
%%
%% Verifies that a basic dependency chain (A -> B -> C) is correctly ordered.
simple_dependency_test() ->
    Graph = digraph:new(),
    digraph:add_vertex(Graph, "a"),
    digraph:add_vertex(Graph, "b"),
    digraph:add_vertex(Graph, "c"),
    % In diameter context: a depends on b, b depends on c
    % So edges: a -> b -> c means c must be compiled first
    digraph:add_edge(Graph, "a", "b"),
    digraph:add_edge(Graph, "b", "c"),

    Sorted = digraph_utils:topsort(Graph),
    digraph:delete(Graph),

    % Topological sort: nodes with no incoming edges first
    % a depends on nothing, so a comes first, then b, then c
    ?assertEqual(["a", "b", "c"], Sorted).

%% @doc Test circular dependency detection.
%%
%% Verifies that circular dependencies are detected (topsort returns false).
circular_dependency_test() ->
    Graph = digraph:new(),
    digraph:add_vertex(Graph, "a"),
    digraph:add_vertex(Graph, "b"),
    digraph:add_edge(Graph, "a", "b"),
    digraph:add_edge(Graph, "b", "a"),

    Result = digraph_utils:topsort(Graph),
    digraph:delete(Graph),

    ?assertEqual(false, Result).

%% @doc Test complex dependency graph.
%%
%% Tests a more complex dependency structure:
%%   d -> b -> a
%%   d -> c -> a
simple_diamond_dependency_test() ->
    Graph = digraph:new(),
    lists:foreach(fun(V) -> digraph:add_vertex(Graph, V) end, ["a", "b", "c", "d"]),
    digraph:add_edge(Graph, "d", "b"),
    digraph:add_edge(Graph, "d", "c"),
    digraph:add_edge(Graph, "b", "a"),
    digraph:add_edge(Graph, "c", "a"),

    Sorted = digraph_utils:topsort(Graph),
    digraph:delete(Graph),

    % 'd' has no incoming edges, so comes first
    % Then 'b' and 'c' (order between them doesn't matter)
    % Finally 'a' which depends on both
    ?assertNotEqual(false, Sorted),
    ?assertEqual(4, length(Sorted)),
    % Check that 'd' comes before 'a', and both 'b' and 'c' come before 'a'
    [First | _] = Sorted,
    Last = lists:last(Sorted),
    ?assertEqual("d", First),
    ?assertEqual("a", Last).

%%%===================================================================
%%% File Pattern Tests
%%%===================================================================

%% @doc Test dia file regex pattern.
%%
%% Verifies the regex used to find .dia files matches correctly.
dia_regex_test_() ->
    Pattern = "^(?!\\._).*\\.dia$",
    {ok, Re} = re:compile(Pattern),
    [
        {"Matches normal .dia file", fun() ->
            ?assertMatch({match, _}, re:run("test.dia", Re))
        end},
        {"Matches path with .dia file", fun() ->
            ?assertMatch({match, _}, re:run("path/to/file.dia", Re))
        end},
        {"Rejects hidden files", fun() ->
            ?assertEqual(nomatch, re:run("._hidden.dia", Re))
        end},
        {"Rejects non-.dia files", fun() ->
            ?assertEqual(nomatch, re:run("test.erl", Re))
        end},
        {"Rejects .dia.bak", fun() ->
            ?assertEqual(nomatch, re:run("test.dia.bak", Re))
        end}
    ].

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

%% @doc Test handling of missing diameter module.
%%
%% Verifies that the code handles cases where diameter application
%% might not be available (though it should be in standard OTP).
diameter_available_test() ->
    % diameter should be available in all supported OTP versions
    ?assertEqual(ok, application:load(diameter)),
    % Just verify the main modules exist
    ?assert(code:which(diameter_dict_util) =/= non_existing).

%%%===================================================================
%%% List Utility Tests
%%%===================================================================

%% @doc Test deduplication logic.
%%
%% Tests the uniq function-like behavior for removing duplicates.
dedup_test_() ->
    [
        {"Empty list", fun() ->
            ?assertEqual([], lists:usort([]))
        end},
        {"Single element", fun() ->
            ?assertEqual(["a"], lists:usort(["a"]))
        end},
        {"No duplicates", fun() ->
            ?assertEqual(["a", "b", "c"], lists:usort(["a", "b", "c"]))
        end},
        {"With duplicates", fun() ->
            Input = ["a", "b", "a", "c", "b"],
            Result = lists:usort(Input),
            ?assertEqual(3, length(Result)),
            ?assert(lists:member("a", Result)),
            ?assert(lists:member("b", Result)),
            ?assert(lists:member("c", Result))
        end},
        {"All same", fun() ->
            ?assertEqual(["x"], lists:usort(["x", "x", "x", "x"]))
        end}
    ].

%%%===================================================================
%%% Integration Setup Tests
%%%===================================================================

%% @doc Test file operations used in setup.
%%
%% Verifies basic file operations work as expected.
file_operations_test_() ->
    {foreach,
        fun() ->
            % Setup: create temp directory
            TmpDir = filename:join([
                "/tmp", "diameter_test_" ++ integer_to_list(erlang:unique_integer([positive]))
            ]),
            ok = filelib:ensure_dir(filename:join(TmpDir, "dummy")),
            TmpDir
        end,
        fun(TmpDir) ->
            % Cleanup: remove temp directory
            file:del_dir_r(TmpDir)
        end,
        [
            fun(TmpDir) ->
                {"Create and verify directory", fun() ->
                    ?assert(filelib:is_dir(TmpDir))
                end}
            end,
            fun(TmpDir) ->
                {"Create nested directories", fun() ->
                    NestedDir = filename:join([TmpDir, "a", "b", "c"]),
                    ok = filelib:ensure_dir(filename:join(NestedDir, "file.txt")),
                    ?assert(filelib:is_dir(filename:join([TmpDir, "a", "b"])))
                end}
            end,
            fun(TmpDir) ->
                {"Write and read file", fun() ->
                    TestFile = filename:join(TmpDir, "test.txt"),
                    Content = "test content",
                    ok = file:write_file(TestFile, Content),
                    {ok, ReadContent} = file:read_file(TestFile),
                    ?assertEqual(list_to_binary(Content), ReadContent)
                end}
            end
        ]}.

%%%===================================================================
%%% Rebar3 Configuration Tests
%%%===================================================================

%% @doc Test rebar3 state operations.
%%
%% Verifies basic rebar3 state manipulation works.
rebar_state_test() ->
    State = rebar_state:new(),
    ?assert(is_tuple(State)),

    % Test setting and getting values
    State1 = rebar_state:set(State, test_key, test_value),
    ?assertEqual(test_value, rebar_state:get(State1, test_key)),

    % Test default value
    ?assertEqual(default, rebar_state:get(State, missing_key, default)).

%% @doc Test provider hooks configuration.
%%
%% Verifies the format expected by rebar3 for provider hooks.
provider_hooks_config_test() ->
    Hooks = [
        {pre, [
            {compile, {diameter, compile}},
            {clean, {diameter, clean}}
        ]}
    ],

    % Verify structure
    ?assertMatch([{pre, _}], Hooks),
    {pre, PreHooks} = hd(Hooks),
    ?assertEqual(2, length(PreHooks)),
    ?assert(lists:member({compile, {diameter, compile}}, PreHooks)),
    ?assert(lists:member({clean, {diameter, clean}}, PreHooks)).
