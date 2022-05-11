%% -------------------------------------------------------------------
%%
%% rebar3 diameter cleaner, in the manner of rebar2.
%%
%% -------------------------------------------------------------------
-module(clean_diameter).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER},             %% The 'user friendly' name of the task
                          {module, ?MODULE},             %% The module implementation of the task
                          {bare,
                           true},                  %% The task can be run by the user, always true
                          {deps, ?DEPS},                 %% The list of dependencies
                          {example, "rebar diameter clean"},   %% How to use the plugin
                          {opts, []},                    %% list of options understood by the plugin
                          {short_desc, "Clean compiled diameter files."},
                          {desc, ""},
                          {namespace, diameter}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Cleaning compiled diameter files...", []),
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    lists:foreach(fun(App) -> clean(State, App) end, Apps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

clean(State, _AppFile) ->
    AppDir = rebar_app_info:dir(_AppFile),
    EbinDir = rebar_app_info:ebin_dir(_AppFile),
    DiaOpts = rebar_state:get(State, dia_opts, []),
    IncludeEbin = proplists:get_value(include, DiaOpts, []),
    code:add_pathsz([EbinDir | IncludeEbin]),
    GeneratedFiles = dia_generated_files(AppDir, "dia", "src", "include", DiaOpts),
    ok = rebar_file_utils:delete_each(GeneratedFiles),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

dia_generated_files(AppDir, DiaDir, SrcDir, IncDir, DiaOpts) ->
    F = fun(File, Acc) ->
           case catch diameter_dict_util:parse({path, File}, DiaOpts) of
               {ok, Spec} ->
                   FileName = dia_filename(File, Spec),
                   [filename:join([AppDir, IncDir, FileName ++ ".hrl"]) | filelib:wildcard(
                                                                              filename:join([AppDir,
                                                                                             SrcDir,
                                                                                             FileName
                                                                                             ++ ".*"]))]
                   ++ Acc;
               _ ->
                   Acc
           end
        end,
    lists:foldl(F,
                [],
                filelib:wildcard(
                    filename:join([AppDir, DiaDir, "*.dia"]))).

dia_filename(File, Spec) ->
    case proplists:get_value(name, Spec) of
        undefined ->
            filename:rootname(
                filename:basename(File));
        Name ->
            Name
    end.
