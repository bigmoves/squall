-module(example@main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/example/main.gleam").
-export([main/0]).

-file("src/example/main.gleam", 4).
-spec main() -> nil.
main() ->
    Result = example@graphql@get_characters:get_characters(
        <<"https://rickandmortyapi.com/graphql"/utf8>>
    ),
    case Result of
        {ok, _} ->
            gleam@io:println(<<"Successfully fetched characters!"/utf8>>);

        {error, Err} ->
            gleam@io:println(<<"Error: "/utf8, Err/binary>>)
    end.
