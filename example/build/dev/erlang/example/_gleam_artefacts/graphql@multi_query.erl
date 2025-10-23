-module(graphql@multi_query).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/graphql/multi_query.gleam").
-export([info_decoder/0, character_decoder/0, characters_decoder/0, location_decoder/0, episode_decoder/0, multi_query_response_decoder/0, multi_query/1]).
-export_type([characters/0, info/0, character/0, location/0, episode/0, multi_query_response/0]).

-type characters() :: {characters,
        gleam@option:option(info()),
        gleam@option:option(list(character()))}.

-type info() :: {info, gleam@option:option(integer())}.

-type character() :: {character, gleam@option:option(binary())}.

-type location() :: {location, gleam@option:option(binary())}.

-type episode() :: {episode, gleam@option:option(binary())}.

-type multi_query_response() :: {multi_query_response,
        gleam@option:option(characters()),
        gleam@option:option(location()),
        gleam@option:option(list(episode()))}.

-file("src/graphql/multi_query.gleam", 31).
-spec info_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok, info()} |
    {error, list(gleam@dynamic:decode_error())}).
info_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"count"/utf8>>,
                gleam@dynamic:optional(fun gleam@dynamic:int/1)
            ))(Data),
            fun(Count) -> {ok, {info, Count}} end
        )
    end.

-file("src/graphql/multi_query.gleam", 45).
-spec character_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok, character()} |
    {error, list(gleam@dynamic:decode_error())}).
character_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"name"/utf8>>,
                gleam@dynamic:optional(fun gleam@dynamic:string/1)
            ))(Data),
            fun(Name) -> {ok, {character, Name}} end
        )
    end.

-file("src/graphql/multi_query.gleam", 13).
-spec characters_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        characters()} |
    {error, list(gleam@dynamic:decode_error())}).
characters_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"info"/utf8>>,
                gleam@dynamic:optional(info_decoder())
            ))(Data),
            fun(Info) ->
                gleam@result:'try'(
                    (gleam@dynamic:field(
                        <<"results"/utf8>>,
                        gleam@dynamic:optional(
                            gleam@dynamic:list(character_decoder())
                        )
                    ))(Data),
                    fun(Results) -> {ok, {characters, Info, Results}} end
                )
            end
        )
    end.

-file("src/graphql/multi_query.gleam", 59).
-spec location_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok, location()} |
    {error, list(gleam@dynamic:decode_error())}).
location_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"id"/utf8>>,
                gleam@dynamic:optional(fun gleam@dynamic:string/1)
            ))(Data),
            fun(Id) -> {ok, {location, Id}} end
        )
    end.

-file("src/graphql/multi_query.gleam", 70).
-spec episode_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok, episode()} |
    {error, list(gleam@dynamic:decode_error())}).
episode_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"id"/utf8>>,
                gleam@dynamic:optional(fun gleam@dynamic:string/1)
            ))(Data),
            fun(Id) -> {ok, {episode, Id}} end
        )
    end.

-file("src/graphql/multi_query.gleam", 85).
-spec multi_query_response_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        multi_query_response()} |
    {error, list(gleam@dynamic:decode_error())}).
multi_query_response_decoder() ->
    fun(Data) ->
        gleam@result:'try'(
            (gleam@dynamic:field(
                <<"characters"/utf8>>,
                gleam@dynamic:optional(characters_decoder())
            ))(Data),
            fun(Characters) ->
                gleam@result:'try'(
                    (gleam@dynamic:field(
                        <<"location"/utf8>>,
                        gleam@dynamic:optional(location_decoder())
                    ))(Data),
                    fun(Location) ->
                        gleam@result:'try'(
                            (gleam@dynamic:field(
                                <<"episodesByIds"/utf8>>,
                                gleam@dynamic:optional(
                                    gleam@dynamic:list(episode_decoder())
                                )
                            ))(Data),
                            fun(Episodes_by_ids) ->
                                {ok,
                                    {multi_query_response,
                                        Characters,
                                        Location,
                                        Episodes_by_ids}}
                            end
                        )
                    end
                )
            end
        )
    end.

-file("src/graphql/multi_query.gleam", 109).
-spec multi_query(binary()) -> {ok, multi_query_response()} | {error, binary()}.
multi_query(Endpoint) ->
    Query = <<"query MultiQuery { characters(page: 2, filter: { name: \"rick\" }) { info { count } results { name } } location(id: 1) { id } episodesByIds(ids: [1, 2]) { id } }"/utf8>>,
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
                                    _pipe@7 = (multi_query_response_decoder())(
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
