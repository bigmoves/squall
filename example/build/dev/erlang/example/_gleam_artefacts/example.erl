-module(example).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/example.gleam").
-export([main/0]).

-file("src/example.gleam", 4).
-spec main() -> nil.
main() ->
    gleam@io:println(<<"Squall Multi-Field Query Example"/utf8>>),
    gleam@io:println(<<"=================================\n"/utf8>>),
    Result = graphql@multi_query:multi_query(
        <<"https://rickandmortyapi.com/graphql"/utf8>>
    ),
    case Result of
        {ok, Response} ->
            gleam@io:println(<<"Response:"/utf8>>),
            gleam@io:debug(Response),
            nil;

        {error, Err} ->
            gleam@io:println(<<"Error: "/utf8, Err/binary>>),
            nil
    end.
