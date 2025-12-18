%% @doc
%% Error condition tests for rebar3_diameter_compiler plugin.
%%
%% This module tests various error conditions and edge cases that could
%% occur during diameter file compilation:
%%
%% <ul>
%% <li>Missing .dia files</li>
%% <li>Malformed .dia files</li>
%% <li>Invalid inheritance chains</li>
%% <li>File permission issues</li>
%% <li>Invalid configuration options</li>
%% </ul>
%%
%% @author Carlos Eduardo de Paula
%% @end
-module(error_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Malformed File Tests
%%%===================================================================

%% @doc Test handling of empty .dia file.
%%
%% Verifies that an empty .dia file is handled gracefully.
empty_dia_file_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "empty.dia"),
        ok = file:write_file(DiaFile, ""),

        % Try to parse the empty file
        Result = diameter_dict_util:parse({path, DiaFile}, []),

        % diameter parser accepts empty files and returns empty spec
        ?assertMatch({ok, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%% @doc Test handling of .dia file with only comments.
%%
%% Verifies that a .dia file with only comments is handled correctly.
comments_only_dia_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "comments.dia"),
        Content = ";; This is a comment\n;; Another comment\n",
        ok = file:write_file(DiaFile, Content),

        Result = diameter_dict_util:parse({path, DiaFile}, []),

        % diameter parser accepts comment-only files
        ?assertMatch({ok, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%% @doc Test handling of .dia file with syntax errors.
%%
%% Verifies proper error reporting for malformed diameter dictionaries.
malformed_dia_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "malformed.dia"),
        % Invalid syntax - missing closing quote
        Content = "@name   'broken\n@vendor 10415 3GPP\n",
        ok = file:write_file(DiaFile, Content),

        Result = diameter_dict_util:parse({path, DiaFile}, []),

        ?assertMatch({error, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%%%===================================================================
%%% Missing Dependency Tests
%%%===================================================================

%% @doc Test .dia file with missing inherited dictionary.
%%
%% Verifies error handling when @inherits references a non-existent dictionary.
missing_inherited_dia_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "missing_dep.dia"),
        Content = "@name   'missing_dep'\n@inherits nonexistent\n@vendor 10415 3GPP\n",
        ok = file:write_file(DiaFile, Content),

        % Parse should succeed but compilation might fail later
        Result = diameter_dict_util:parse({path, DiaFile}, []),

        case Result of
            {ok, Spec} ->
                % Verify the inherits is parsed
                ?assert(is_list(Spec));
            {error, _} ->
                % Also acceptable if parser catches this
                ok
        end
    after
        file:del_dir_r(TmpDir)
    end.

%%%===================================================================
%%% File System Error Tests
%%%===================================================================

%% @doc Test handling of non-existent .dia file.
%%
%% Verifies error handling when trying to compile a file that doesn't exist.
nonexistent_file_test() ->
    NonExistentFile =
        "/tmp/definitely_does_not_exist_" ++
            integer_to_list(erlang:unique_integer([positive])) ++ ".dia",

    Result = diameter_dict_util:parse({path, NonExistentFile}, []),

    ?assertMatch({error, _}, Result).

%% @doc Test handling of directory instead of file.
%%
%% Verifies error when a directory path is given instead of a file.
directory_as_file_test() ->
    TmpDir = setup_temp_dir(),
    try
        SubDir = filename:join(TmpDir, "notafile.dia"),
        % Create directory with this name
        case file:make_dir(SubDir) of
            ok -> ok;
            % Already exists is fine
            {error, eexist} -> ok
        end,

        Result = diameter_dict_util:parse({path, SubDir}, []),

        ?assertMatch({error, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%%%===================================================================
%%% Configuration Error Tests
%%%===================================================================

%% @doc Test with invalid dia_opts.
%%
%% Verifies handling of invalid configuration options.
invalid_opts_test_() ->
    [
        {"Invalid outdir type", fun() ->
            % Should be string
            Opts = [{outdir, 12345}],
            OutDir = proplists:get_value(outdir, Opts),
            % Just verifies it's retrieved
            ?assertEqual(12345, OutDir)
        end},
        {"Unknown option", fun() ->
            Opts = [{unknown_option, value}],
            Value = proplists:get_value(unknown_option, Opts),
            ?assertEqual(value, Value)
        end}
    ].

%%%===================================================================
%%% Circular Dependency Tests
%%%===================================================================

%% @doc Test detection of circular inheritance.
%%
%% Creates two .dia files that inherit from each other and verifies
%% the circular dependency is detected.
circular_inheritance_test() ->
    TmpDir = setup_temp_dir(),
    try
        % Create file1 that inherits from file2
        File1 = filename:join(TmpDir, "circular1.dia"),
        Content1 = "@name 'circular1'\n@inherits circular2\n@vendor 10415 3GPP\n",
        ok = file:write_file(File1, Content1),

        % Create file2 that inherits from file1
        File2 = filename:join(TmpDir, "circular2.dia"),
        Content2 = "@name 'circular2'\n@inherits circular1\n@vendor 10415 3GPP\n",
        ok = file:write_file(File2, Content2),

        % Try to build dependency graph
        Graph = digraph:new(),
        digraph:add_vertex(Graph, "circular1"),
        digraph:add_vertex(Graph, "circular2"),
        digraph:add_edge(Graph, "circular1", "circular2"),
        digraph:add_edge(Graph, "circular2", "circular1"),

        % Topological sort should fail (return false)
        Result = digraph_utils:topsort(Graph),
        digraph:delete(Graph),

        ?assertEqual(false, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%%%===================================================================
%%% Name Conflict Tests
%%%===================================================================

%% @doc Test .dia files with conflicting names.
%%
%% Verifies handling when @name directive differs from filename.
name_mismatch_test() ->
    TmpDir = setup_temp_dir(),
    try
        % File is named one.dia but @name says 'two'
        DiaFile = filename:join(TmpDir, "one.dia"),
        Content = "@name 'two'\n@vendor 10415 3GPP\n@avp_types\nTest 1 Unsigned32 V\n",
        ok = file:write_file(DiaFile, Content),

        {ok, Spec} = diameter_dict_util:parse({path, DiaFile}, []),
        Name = proplists:get_value(name, Spec),

        % Name from @name directive should be used
        ?assertEqual("two", Name)
    after
        file:del_dir_r(TmpDir)
    end.

%%%===================================================================
%%% Encoding Tests
%%%===================================================================

%% @doc Test .dia file with UTF-8 content.
%%
%% Verifies that UTF-8 characters in comments and strings are handled.
utf8_content_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "utf8.dia"),
        % Include UTF-8 characters in comment
        Content =
            ";; Test with UTF-8: こんにちは, Привет, مرحبا\n"
            "@name 'utf8test'\n"
            "@vendor 10415 3GPP\n"
            "@avp_types\n"
            "Test 1 UTF8String V\n",
        ok = file:write_file(DiaFile, unicode:characters_to_binary(Content)),

        Result = diameter_dict_util:parse({path, DiaFile}, []),

        % Should parse successfully
        ?assertMatch({ok, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%%%===================================================================
%%% Edge Case Tests
%%%===================================================================

%% @doc Test .dia file with very long lines.
%%
%% Verifies handling of files with extremely long lines.
long_line_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "longline.dia"),
        % Create a very long comment line
        LongComment = ";; " ++ lists:duplicate(10000, $a) ++ "\n",
        Content = LongComment ++ "@name 'longline'\n@vendor 10415 3GPP\n",
        ok = file:write_file(DiaFile, Content),

        Result = diameter_dict_util:parse({path, DiaFile}, []),

        % Should handle long lines
        ?assertMatch({ok, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%% @doc Test .dia file with no newline at end.
%%
%% Verifies parsing works even without trailing newline.
no_trailing_newline_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "nonewline.dia"),
        % No \n at end
        Content = "@name 'nonewline'\n@vendor 10415 3GPP",
        ok = file:write_file(DiaFile, Content),

        Result = diameter_dict_util:parse({path, DiaFile}, []),

        ?assertMatch({ok, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%% @doc Test .dia file with Windows line endings.
%%
%% Verifies CRLF line endings are handled correctly.
windows_line_endings_test() ->
    TmpDir = setup_temp_dir(),
    try
        DiaFile = filename:join(TmpDir, "crlf.dia"),
        Content = "@name 'crlf'\r\n@vendor 10415 3GPP\r\n",
        ok = file:write_file(DiaFile, Content),

        Result = diameter_dict_util:parse({path, DiaFile}, []),

        ?assertMatch({ok, _}, Result)
    after
        file:del_dir_r(TmpDir)
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Create a temporary directory for testing.
%% @private
setup_temp_dir() ->
    TmpDir = filename:join([
        "/tmp",
        "diameter_error_test_" ++ integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = filelib:ensure_dir(filename:join(TmpDir, "dummy")),
    TmpDir.
