-module(example_with_vars).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/example_with_vars.gleam").
-export([main/0]).

-file("src/example_with_vars.gleam", 4).
-spec main() -> nil.
main() ->
    gleam@io:println(
        <<"Squall Multi-Field Query Example (with Variables)"/utf8>>
    ),
    gleam@io:println(
        <<"==================================================\n"/utf8>>
    ),
    gleam@io:println(<<"Calling multi_query_with_vars with:"/utf8>>),
    gleam@io:println(<<"  page: 2"/utf8>>),
    gleam@io:println(<<"  name: \"rick\""/utf8>>),
    gleam@io:println(<<"  locationId: \"1\""/utf8>>),
    gleam@io:println(<<"  episodeIds: [1, 2]\n"/utf8>>),
    Result = graphql@multi_query_with_vars:multi_query_with_vars(
        <<"https://rickandmortyapi.com/graphql"/utf8>>,
        2,
        <<"rick"/utf8>>,
        <<"1"/utf8>>,
        [1, 2]
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
