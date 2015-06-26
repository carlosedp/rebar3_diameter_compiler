-module(rebar3_diameter_compiler).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================

init(State) ->
    {ok, State1} = compile_diameter:init(State),
    {ok, State2} = clean_diameter:init(State1),
    {ok, State2}.

do(State) ->
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
