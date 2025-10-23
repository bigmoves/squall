-module(graphql@get_character).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/graphql/get_character.gleam").
-export([character_decoder/0, get_character_response_decoder/0, get_character/2]).
-export_type([character/0, get_character_response/0]).

-type character() :: {character,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type get_character_response() :: {get_character_response,
        gleam@option:option(character())}.

-file("src/graphql/get_character.gleam", 20).
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
                                        gleam@result:'try'(
                                            (gleam@dynamic:field(
                                                <<"type"/utf8>>,
                                                gleam@dynamic:optional(
                                                    fun gleam@dynamic:string/1
                                                )
                                            ))(Data),
                                            fun(Type_) ->
                                                gleam@result:'try'(
                                                    (gleam@dynamic:field(
                                                        <<"gender"/utf8>>,
                                                        gleam@dynamic:optional(
                                                            fun gleam@dynamic:string/1
                                                        )
                                                    ))(Data),
                                                    fun(Gender) ->
                                                        {ok,
                                                            {character,
                                                                Id,
                                                                Name,
                                                                Status,
                                                                Species,
                                                                Type_,
                                                                Gender}}
                                                    end
                                                )
                                            end
                                        )
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )
    end.

-file("src/graphql/get_character.gleam", 60).
-spec get_character_response_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        get_character_response()} |
    {error, list(gleam@dynamic:decode_error())}).
get_character_response_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"character"/utf8>>,
                gleam@dynamic:optional(character_decoder())
            ))(Data),
            fun(Character) -> {ok, {get_character_response, Character}} end
        )
    end.

-file("src/graphql/get_character.gleam", 73).
-spec get_character(binary(), binary()) -> {ok, get_character_response()} |
    {error, binary()}.
get_character(Endpoint, Id) ->
    Query = <<"query GetCharacter($id: ID!) { character { id name status species type gender } }"/utf8>>,
    Variables = gleam@json:object([{<<"id"/utf8>>, gleam@json:string(Id)}]),
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
                                    _pipe@7 = (get_character_response_decoder())(
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
