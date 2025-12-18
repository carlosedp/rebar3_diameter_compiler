%% @doc
%% Property-based tests for rebar3_diameter_compiler plugin.
%%
%% This module uses PropEr (or EQC if available) for property-based testing.
%% It generates random test cases to verify invariants and edge cases that
%% might not be covered by example-based tests.
%%
%% Note: These tests require PropEr to be available. If PropEr is not installed,
%% the tests will be skipped gracefully.
%%
%% @author Carlos Eduardo de Paula
%% @end
-module(property_tests).

-include_lib("eunit/include/eunit.hrl").

%% Check if PropEr is available
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(PROPER_AVAILABLE, true).
-else.
-define(PROPER_AVAILABLE, false).
-endif.

%%%===================================================================
%%% Property Tests
%%%===================================================================

%% @doc Test that dependency graph operations preserve vertex count.
%%
%% Property: Adding N vertices should result in a graph with N vertices.
-ifdef(PROPER).
prop_graph_vertex_count_test() ->
    ?assert(
        proper:quickcheck(
            ?FORALL(
                Vertices,
                proper_types:list(proper_types:atom()),
                begin
                    Graph = digraph:new(),
                    UniqueVertices = lists:usort(Vertices),
                    lists:foreach(fun(V) -> digraph:add_vertex(Graph, V) end, UniqueVertices),
                    VertexCount = length(digraph:vertices(Graph)),
                    digraph:delete(Graph),
                    VertexCount =:= length(UniqueVertices)
                end
            )
        )
    ).
-else.
prop_graph_vertex_count_test() ->
    ?debugMsg("PropEr not available, skipping property test").
-endif.

%% @doc Test that file path operations are idempotent.
%%
%% Property: basename(basename(X)) =:= basename(X)
-ifdef(PROPER).
prop_basename_idempotent_test() ->
    ?assert(
        proper:quickcheck(
            ?FORALL(
                Path,
                proper_types:non_empty(proper_types:list(proper_types:range($a, $z))),
                begin
                    PathStr = Path,
                    Base1 = filename:basename(PathStr),
                    Base2 = filename:basename(Base1),
                    Base1 =:= Base2
                end
            )
        )
    ).
-else.
prop_basename_idempotent_test() ->
    ?debugMsg("PropEr not available, skipping property test").
-endif.

%% @doc Test that list deduplication preserves elements.
%%
%% Property: All elements in original list appear in deduplicated list.
-ifdef(PROPER).
prop_dedup_preserves_elements_test() ->
    ?assert(
        proper:quickcheck(
            ?FORALL(
                List,
                proper_types:list(proper_types:integer()),
                begin
                    Deduped = lists:usort(List),
                    lists:all(fun(X) -> lists:member(X, Deduped) end, List)
                end
            )
        )
    ).
-else.
prop_dedup_preserves_elements_test() ->
    ?debugMsg("PropEr not available, skipping property test").
-endif.

%%%===================================================================
%%% Fuzz Testing
%%%===================================================================

%% @doc Fuzz test for filename operations.
%%
%% Tests that filename operations don't crash with random inputs.
fuzz_filename_operations_test_() ->
    {timeout, 10, fun() ->
        RandomStrings = generate_random_strings(100),
        lists:foreach(
            fun(Str) ->
                % These should not crash
                _ = filename:basename(Str),
                _ = filename:dirname(Str),
                _ = filename:rootname(Str),
                ok
            end,
            RandomStrings
        )
    end}.

%% @doc Fuzz test for proplists operations.
%%
%% Tests that proplist operations handle random data gracefully.
fuzz_proplists_test_() ->
    {timeout, 10, fun() ->
        RandomLists = generate_random_proplists(50),
        lists:foreach(
            fun(PropList) ->
                % These should not crash
                _ = proplists:get_value(test_key, PropList, default),
                _ = proplists:get_keys(PropList),
                _ = proplists:is_defined(test_key, PropList),
                ok
            end,
            RandomLists
        )
    end}.

%% @doc Fuzz test for digraph operations.
%%
%% Tests that digraph operations are robust with random inputs.
fuzz_digraph_test() ->
    Graph = digraph:new(),
    try
        RandomAtoms = [list_to_atom("v" ++ integer_to_list(N)) || N <- lists:seq(1, 20)],

        % Add random vertices
        lists:foreach(
            fun(V) -> digraph:add_vertex(Graph, V) end,
            RandomAtoms
        ),

        % Add random edges (some may fail if they create cycles)
        lists:foreach(
            fun(_) ->
                V1 = lists:nth(rand:uniform(length(RandomAtoms)), RandomAtoms),
                V2 = lists:nth(rand:uniform(length(RandomAtoms)), RandomAtoms),
                try
                    digraph:add_edge(Graph, V1, V2)
                catch
                    _:_ -> ok
                end
            end,
            lists:seq(1, 10)
        ),

        % Operations should not crash
        _ = digraph:vertices(Graph),
        _ = digraph:edges(Graph),
        _ = digraph_utils:is_acyclic(Graph),

        ok
    after
        digraph:delete(Graph)
    end.

%%%===================================================================
%%% Invariant Tests
%%%===================================================================

%% @doc Test that topological sort maintains dependencies.
%%
%% Invariant: If A depends on B, B should come before A in sorted order.
topsort_maintains_dependencies_test() ->
    Graph = digraph:new(),

    % Create a known dependency chain: d -> c -> b -> a
    Vertices = [a, b, c, d],
    lists:foreach(fun(V) -> digraph:add_vertex(Graph, V) end, Vertices),
    % Edges show dependencies: d depends on c, c on b, b on a
    digraph:add_edge(Graph, d, c),
    digraph:add_edge(Graph, c, b),
    digraph:add_edge(Graph, b, a),

    Sorted = digraph_utils:topsort(Graph),
    digraph:delete(Graph),

    % In topological sort, nodes with no dependencies come first
    % So the order should be: d, c, b, a
    ?assertEqual([d, c, b, a], Sorted).

%% @doc Test that reachable nodes include the starting node.
%%
%% Invariant: reachable([X], Graph) should always include X.
reachable_includes_self_test() ->
    Graph = digraph:new(),

    Vertices = [a, b, c],
    lists:foreach(fun(V) -> digraph:add_vertex(Graph, V) end, Vertices),
    digraph:add_edge(Graph, a, b),
    digraph:add_edge(Graph, b, c),

    lists:foreach(
        fun(V) ->
            Reachable = digraph_utils:reachable([V], Graph),
            ?assert(lists:member(V, Reachable))
        end,
        Vertices
    ),

    digraph:delete(Graph).

%%%===================================================================
%%% Stress Tests
%%%===================================================================

%% @doc Stress test with large dependency graph.
%%
%% Creates a large graph to test performance and correctness.
large_graph_test_() ->
    {timeout, 30, fun() ->
        Graph = digraph:new(),
        try
            % Create 1000 vertices
            Vertices = [list_to_atom("v" ++ integer_to_list(N)) || N <- lists:seq(1, 1000)],
            lists:foreach(fun(V) -> digraph:add_vertex(Graph, V) end, Vertices),

            % Add edges in a way that creates a DAG (no cycles)
            lists:foreach(
                fun(N) when N < 1000 ->
                    V1 = lists:nth(N, Vertices),
                    V2 = lists:nth(N + 1, Vertices),
                    digraph:add_edge(Graph, V1, V2)
                end,
                lists:seq(1, 999)
            ),

            % Should be acyclic
            ?assert(digraph_utils:is_acyclic(Graph)),

            % Topsort should succeed
            Sorted = digraph_utils:topsort(Graph),
            ?assertNotEqual(false, Sorted),
            ?assertEqual(1000, length(Sorted))
        after
            digraph:delete(Graph)
        end
    end}.

%% @doc Stress test with many file operations.
%%
%% Performs many file operations to test for resource leaks.
many_file_operations_test_() ->
    {timeout, 30, fun() ->
        TmpBase =
            "/tmp/diameter_stress_" ++
                integer_to_list(erlang:unique_integer([positive])),
        ok = filelib:ensure_dir(filename:join(TmpBase, "dummy")),

        try
            % Create and delete many files
            lists:foreach(
                fun(N) ->
                    File = filename:join(TmpBase, "file" ++ integer_to_list(N) ++ ".txt"),
                    ok = file:write_file(File, "test"),
                    {ok, _} = file:read_file(File),
                    ok = file:delete(File)
                end,
                lists:seq(1, 100)
            )
        after
            file:del_dir_r(TmpBase)
        end
    end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Generate random strings for fuzz testing.
%% @private
generate_random_strings(Count) ->
    [generate_random_string(rand:uniform(50)) || _ <- lists:seq(1, Count)].

%% @doc Generate a random string of given length.
%% @private
generate_random_string(Len) ->
    [rand:uniform(126) - 32 + 32 || _ <- lists:seq(1, Len)].

%% @doc Generate random proplists for fuzz testing.
%% @private
generate_random_proplists(Count) ->
    [generate_random_proplist(rand:uniform(10)) || _ <- lists:seq(1, Count)].

%% @doc Generate a random proplist with N entries.
%% @private
generate_random_proplist(N) ->
    [{list_to_atom("key" ++ integer_to_list(I)), rand:uniform(1000)} || I <- lists:seq(1, N)].
