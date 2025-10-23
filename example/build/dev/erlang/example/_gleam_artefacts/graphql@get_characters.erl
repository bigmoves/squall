-module(graphql@get_characters).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/graphql/get_characters.gleam").
-export([character_decoder/0, characters_decoder/0, get_characters_response_decoder/0, get_characters/1]).
-export_type([characters/0, character/0, get_characters_response/0]).

-type characters() :: {characters, gleam@option:option(list(character()))}.

-type character() :: {character,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type get_characters_response() :: {get_characters_response,
        gleam@option:option(characters())}.

-file("src/graphql/get_characters.gleam", 32).
-spec character_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok, character()} |
    {error, list(gleam@dynamic:decode_error())}).
character_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"id"/utf8>>,
                gleam@dynamic:optional(fun gleam@dynamic:string/1)
            ))(Data),
            fun(Id) ->
                gleam@result:'try'(
                    (gleam@dynamic:field(
                        <<"name"/utf8>>,
                        gleam@dynamic:optional(fun gleam@dynamic:string/1)
                    ))(Data),
                    fun(Name) ->
                        gleam@result:'try'(
                            (gleam@dynamic:field(
                                <<"status"/utf8>>,
                                gleam@dynamic:optional(
                                    fun gleam@dynamic:string/1
                                )
                            ))(Data),
                            fun(Status) ->
                                gleam@result:'try'(
                                    (gleam@dynamic:field(
                                        <<"species"/utf8>>,
                                        gleam@dynamic:optional(
                                            fun gleam@dynamic:string/1
                                        )
                                    ))(Data),
                                    fun(Species) ->
                                        {ok,
                                            {character,
                                                Id,
                                                Name,
                                                Status,
                                                Species}}
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )
    end.

-file("src/graphql/get_characters.gleam", 13).
-spec characters_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        characters()} |
    {error, list(gleam@dynamic:decode_error())}).
characters_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"results"/utf8>>,
                gleam@dynamic:optional(gleam@dynamic:list(character_decoder()))
            ))(Data),
            fun(Results) -> {ok, {characters, Results}} end
        )
    end.

-file("src/graphql/get_characters.gleam", 55).
-spec get_characters_response_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        get_characters_response()} |
    {error, list(gleam@dynamic:decode_error())}).
get_characters_response_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"characters"/utf8>>,
                gleam@dynamic:optional(characters_decoder())
            ))(Data),
            fun(Characters) -> {ok, {get_characters_response, Characters}} end
        )
    end.

-file("src/graphql/get_characters.gleam", 65).
-spec get_characters(binary()) -> {ok, get_characters_response()} |
    {error, binary()}.
get_characters(Endpoint) ->
    Query = <<"query GetCharacters { characters { results { id name status species } } }"/utf8>>,
    Variables = gleam@json:object([]),
    Body = gleam@json:object(
        [{<<"query"/utf8>>, gleam@json:string(Query)},
            {<<"variables"/utf8>>, Variables}]
    ),
    gleam@result:'try'(
        begin
            _pipe = gleam@http@request:to(Endpoint),
            gleam@result:map_error(
                _pipe,
                fun(_) -> <<"Invalid endpoint URL"/utf8>> end
            )
        end,
        fun(Req) ->
            Req@1 = begin
                _pipe@1 = Req,
                _pipe@2 = gleam@http@request:set_method(_pipe@1, post),
                _pipe@3 = gleam@http@request:set_body(
                    _pipe@2,
                    gleam@json:to_string(Body)
                ),
                gleam@http@request:set_header(
                    _pipe@3,
                    <<"content-type"/utf8>>,
                    <<"application/json"/utf8>>
                )
            end,
            gleam@result:'try'(
                begin
                    _pipe@4 = gleam@httpc:send(Req@1),
                    gleam@result:map_error(
                        _pipe@4,
                        fun(_) -> <<"HTTP request failed"/utf8>> end
                    )
                end,
                fun(Resp) ->
                    gleam@result:'try'(
                        begin
                            _pipe@5 = gleam@json:decode(
                                erlang:element(4, Resp),
                                fun gleam@dynamic:dynamic/1
                            ),
                            gleam@result:map_error(
                                _pipe@5,
                                fun(_) ->
                                    <<"Failed to decode JSON response"/utf8>>
                                end
                            )
                        end,
                        fun(Json_value) ->
                            gleam@result:'try'(
                                begin
                                    _pipe@6 = (gleam@dynamic:field(
                                        <<"data"/utf8>>,
                                        fun gleam@dynamic:dynamic/1
                                    ))(Json_value),
                                    gleam@result:map_error(
                                        _pipe@6,
                                        fun(_) ->
                                            <<"No data field in response"/utf8>>
                                        end
                                    )
                                end,
                                fun(Data_field) ->
                                    _pipe@7 = (get_characters_response_decoder())(
                                        Data_field
                                    ),
                                    gleam@result:map_error(
                                        _pipe@7,
                                        fun(_) ->
                                            <<"Failed to decode response data"/utf8>>
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
