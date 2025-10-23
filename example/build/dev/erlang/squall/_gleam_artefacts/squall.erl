-module(squall).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/squall.gleam").
-export([main/0]).

-file("src/squall.gleam", 28).
-spec print_usage() -> nil.
print_usage() ->
    gleam@io:println(
        <<"
Squall - Type-safe GraphQL client generator for Gleam

Usage:
  gleam run -m squall generate <endpoint>
  gleam run -m squall generate              # Uses GRAPHQL_ENDPOINT env var

Commands:
  generate <endpoint>   Generate Gleam code from .gql files

The tool will:
  1. Find all .gql files in src/**/graphql/ directories
  2. Introspect the GraphQL schema from the endpoint
  3. Generate type-safe Gleam functions for each query/mutation/subscription

Example:
  gleam run -m squall generate https://rickandmortyapi.com/graphql
"/utf8>>
    ).

-file("src/squall.gleam", 51).
-spec generate_with_env() -> nil.
generate_with_env() ->
    gleam@io:println(
        <<"Error: GRAPHQL_ENDPOINT environment variable not set"/utf8>>
    ),
    gleam@io:println(<<"Usage: gleam run -m squall generate <endpoint>"/utf8>>),
    nil.

-file("src/squall.gleam", 258).
-spec int_to_string(integer()) -> binary().
int_to_string(I) ->
    case I of
        0 ->
            <<"0"/utf8>>;

        1 ->
            <<"1"/utf8>>;

        2 ->
            <<"2"/utf8>>;

        3 ->
            <<"3"/utf8>>;

        4 ->
            <<"4"/utf8>>;

        5 ->
            <<"5"/utf8>>;

        6 ->
            <<"6"/utf8>>;

        7 ->
            <<"7"/utf8>>;

        8 ->
            <<"8"/utf8>>;

        9 ->
            <<"9"/utf8>>;

        _ ->
            S = I,
            case S >= 0 of
                true ->
                    <<(int_to_string(S div 10))/binary,
                        (int_to_string(S rem 10))/binary>>;

                false ->
                    <<"-"/utf8, (int_to_string(- S))/binary>>
            end
    end.

-file("src/squall.gleam", 211).
-spec make_graphql_request(binary(), binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
make_graphql_request(Endpoint, Query, Variables) ->
    Vars_value = case Variables of
        <<""/utf8>> ->
            gleam@json:object([]);

        _ ->
            gleam@json:string(Variables)
    end,
    Body = begin
        _pipe = gleam@json:object(
            [{<<"query"/utf8>>, gleam@json:string(Query)},
                {<<"variables"/utf8>>, Vars_value}]
        ),
        gleam@json:to_string(_pipe)
    end,
    gleam@result:'try'(
        begin
            _pipe@1 = gleam@http@request:to(Endpoint),
            gleam@result:map_error(
                _pipe@1,
                fun(_) -> <<"Invalid endpoint URL: "/utf8, Endpoint/binary>> end
            )
        end,
        fun(Req) ->
            Req@1 = begin
                _pipe@2 = Req,
                _pipe@3 = gleam@http@request:set_method(_pipe@2, post),
                _pipe@4 = gleam@http@request:set_body(_pipe@3, Body),
                _pipe@5 = gleam@http@request:set_header(
                    _pipe@4,
                    <<"content-type"/utf8>>,
                    <<"application/json"/utf8>>
                ),
                gleam@http@request:set_header(
                    _pipe@5,
                    <<"accept"/utf8>>,
                    <<"application/json"/utf8>>
                )
            end,
            gleam@result:'try'(
                begin
                    _pipe@6 = gleam@httpc:send(Req@1),
                    gleam@result:map_error(
                        _pipe@6,
                        fun(_) ->
                            <<"Failed to send HTTP request to "/utf8,
                                Endpoint/binary>>
                        end
                    )
                end,
                fun(Resp) -> case erlang:element(2, Resp) of
                        200 ->
                            {ok, erlang:element(4, Resp)};

                        _ ->
                            {error,
                                <<<<<<"HTTP request failed with status "/utf8,
                                            (int_to_string(
                                                erlang:element(2, Resp)
                                            ))/binary>>/binary,
                                        ": "/utf8>>/binary,
                                    (erlang:element(4, Resp))/binary>>}
                    end end
            )
        end
    ).

-file("src/squall.gleam", 133).
-spec introspect_schema(binary()) -> {ok, squall@internal@schema:schema()} |
    {error, squall@internal@error:error()}.
introspect_schema(Endpoint) ->
    Introspection_query = <<"
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          name
          kind
          description
          fields {
            name
            description
            type {
              ...TypeRef
            }
            args {
              name
              type {
                ...TypeRef
              }
            }
          }
          inputFields {
            name
            type {
              ...TypeRef
            }
          }
          enumValues {
            name
          }
          possibleTypes {
            name
          }
        }
      }
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    }
  "/utf8>>,
    gleam@result:'try'(
        begin
            _pipe = make_graphql_request(
                Endpoint,
                Introspection_query,
                <<""/utf8>>
            ),
            gleam@result:map_error(
                _pipe,
                fun(Err) -> {http_request_failed, Err} end
            )
        end,
        fun(Response) ->
            squall@internal@schema:parse_introspection_response(Response)
        end
    ).

-file("src/squall.gleam", 57).
-spec generate(binary()) -> nil.
generate(Endpoint) ->
    gleam@io:println(<<"🌊 Squall - GraphQL Code Generator"/utf8>>),
    gleam@io:println(<<"================================\n"/utf8>>),
    gleam@io:println(
        <<"📡 Introspecting GraphQL schema from: "/utf8, Endpoint/binary>>
    ),
    case introspect_schema(Endpoint) of
        {ok, Schema_data} ->
            gleam@io:println(<<"✓ Schema introspected successfully\n"/utf8>>),
            gleam@io:println(<<"🔍 Discovering .gql files..."/utf8>>),
            case squall@internal@discovery:find_graphql_files(<<"src"/utf8>>) of
                {ok, Files} ->
                    gleam@io:println(
                        <<<<"✓ Found "/utf8,
                                (int_to_string(erlang:length(Files)))/binary>>/binary,
                            " .gql file(s)\n"/utf8>>
                    ),
                    gleam@list:each(
                        Files,
                        fun(File) ->
                            gleam@io:println(
                                <<"📝 Processing: "/utf8,
                                    (erlang:element(2, File))/binary>>
                            ),
                            case squall@internal@parser:parse(
                                erlang:element(4, File)
                            ) of
                                {ok, Operation} ->
                                    case squall@internal@codegen:generate_operation(
                                        erlang:element(3, File),
                                        Operation,
                                        Schema_data,
                                        Endpoint
                                    ) of
                                        {ok, Code} ->
                                            Output_path = gleam@string:replace(
                                                erlang:element(2, File),
                                                <<".gql"/utf8>>,
                                                <<".gleam"/utf8>>
                                            ),
                                            case simplifile:write(
                                                Output_path,
                                                Code
                                            ) of
                                                {ok, _} ->
                                                    gleam@io:println(
                                                        <<"  ✓ Generated: "/utf8,
                                                            Output_path/binary>>
                                                    );

                                                {error, _} ->
                                                    gleam@io:println(
                                                        <<"  ✗ Failed to write: "/utf8,
                                                            Output_path/binary>>
                                                    )
                                            end;

                                        {error, Err} ->
                                            gleam@io:println(
                                                <<"  ✗ Code generation failed: "/utf8,
                                                    (squall@internal@error:to_string(
                                                        Err
                                                    ))/binary>>
                                            )
                                    end;

                                {error, Err@1} ->
                                    gleam@io:println(
                                        <<"  ✗ Parse failed: "/utf8,
                                            (squall@internal@error:to_string(
                                                Err@1
                                            ))/binary>>
                                    )
                            end
                        end
                    ),
                    gleam@io:println(<<"\n✨ Code generation complete!"/utf8>>),
                    nil;

                {error, Err@2} ->
                    gleam@io:println(
                        <<"✗ Failed to discover files: "/utf8,
                            (squall@internal@error:to_string(Err@2))/binary>>
                    ),
                    nil
            end;

        {error, Err@3} ->
            gleam@io:println(
                <<"✗ Schema introspection failed: "/utf8,
                    (squall@internal@error:to_string(Err@3))/binary>>
            ),
            nil
    end.

-file("src/squall.gleam", 17).
-spec main() -> nil.
main() ->
    case erlang:element(4, argv:load()) of
        [<<"generate"/utf8>>, Endpoint] ->
            generate(Endpoint);

        [<<"generate"/utf8>>] ->
            generate_with_env();

        _ ->
            print_usage(),
            nil
    end.
