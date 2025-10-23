-module(squall@internal@schema).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/squall/internal/schema.gleam").
-export([get_type_count/1, find_type/2, get_type_name/1, parse_introspection_response/1, get_type_fields/1, get_query_type_name/1]).
-export_type([type_kind/0, type_ref/0, field/0, input_value/0, type/0, schema/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type type_kind() :: scalar |
    object |
    interface |
    union |
    enum |
    input_object |
    list |
    non_null.

-type type_ref() :: {named_type, binary(), type_kind()} |
    {list_type, type_ref()} |
    {non_null_type, type_ref()}.

-type field() :: {field,
        binary(),
        type_ref(),
        list(input_value()),
        gleam@option:option(binary())}.

-type input_value() :: {input_value,
        binary(),
        type_ref(),
        gleam@option:option(binary())}.

-type type() :: {scalar_type, binary(), gleam@option:option(binary())} |
    {object_type, binary(), list(field()), gleam@option:option(binary())} |
    {interface_type, binary(), list(field()), gleam@option:option(binary())} |
    {union_type, binary(), list(binary()), gleam@option:option(binary())} |
    {enum_type, binary(), list(binary()), gleam@option:option(binary())} |
    {input_object_type,
        binary(),
        list(input_value()),
        gleam@option:option(binary())}.

-type schema() :: {schema,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), type())}.

-file("src/squall/internal/schema.gleam", 301).
?DOC(false).
-spec kind_from_string(binary()) -> type_kind().
kind_from_string(S) ->
    case S of
        <<"SCALAR"/utf8>> ->
            scalar;

        <<"OBJECT"/utf8>> ->
            object;

        <<"INTERFACE"/utf8>> ->
            interface;

        <<"UNION"/utf8>> ->
            union;

        <<"ENUM"/utf8>> ->
            enum;

        <<"INPUT_OBJECT"/utf8>> ->
            input_object;

        <<"LIST"/utf8>> ->
            list;

        <<"NON_NULL"/utf8>> ->
            non_null;

        _ ->
            scalar
    end.

-file("src/squall/internal/schema.gleam", 319).
?DOC(false).
-spec errors_to_string(list(gleam@dynamic:decode_error())) -> binary().
errors_to_string(Errs) ->
    _pipe = Errs,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(Err) -> <<"Expected "/utf8, (erlang:element(2, Err))/binary>> end
    ),
    _pipe@2 = gleam@list:reduce(
        _pipe@1,
        fun(A, B) -> <<<<A/binary, ", "/utf8>>/binary, B/binary>> end
    ),
    gleam@result:unwrap(_pipe@2, <<"unknown error"/utf8>>).

-file("src/squall/internal/schema.gleam", 315).
?DOC(false).
-spec to_schema_error(list(gleam@dynamic:decode_error())) -> squall@internal@error:error().
to_schema_error(Errs) ->
    {invalid_schema_response,
        <<"Decode error: "/utf8, (errors_to_string(Errs))/binary>>}.

-file("src/squall/internal/schema.gleam", 266).
?DOC(false).
-spec decode_type_ref(gleam@dynamic:dynamic_()) -> {ok, type_ref()} |
    {error, squall@internal@error:error()}.
decode_type_ref(Dyn) ->
    gleam@result:'try'(
        begin
            _pipe = (gleam@dynamic:field(
                <<"kind"/utf8>>,
                fun gleam@dynamic:string/1
            ))(Dyn),
            gleam@result:map_error(_pipe, fun to_schema_error/1)
        end,
        fun(Kind) -> case Kind of
                <<"NON_NULL"/utf8>> ->
                    gleam@result:'try'(
                        begin
                            _pipe@1 = (gleam@dynamic:field(
                                <<"ofType"/utf8>>,
                                fun gleam@dynamic:dynamic/1
                            ))(Dyn),
                            gleam@result:map_error(
                                _pipe@1,
                                fun to_schema_error/1
                            )
                        end,
                        fun(Of_type_dyn) ->
                            gleam@result:'try'(
                                decode_type_ref(Of_type_dyn),
                                fun(Of_type) ->
                                    {ok, {non_null_type, Of_type}}
                                end
                            )
                        end
                    );

                <<"LIST"/utf8>> ->
                    gleam@result:'try'(
                        begin
                            _pipe@2 = (gleam@dynamic:field(
                                <<"ofType"/utf8>>,
                                fun gleam@dynamic:dynamic/1
                            ))(Dyn),
                            gleam@result:map_error(
                                _pipe@2,
                                fun to_schema_error/1
                            )
                        end,
                        fun(Of_type_dyn@1) ->
                            gleam@result:'try'(
                                decode_type_ref(Of_type_dyn@1),
                                fun(Of_type@1) ->
                                    {ok, {list_type, Of_type@1}}
                                end
                            )
                        end
                    );

                _ ->
                    gleam@result:'try'(
                        begin
                            _pipe@3 = (gleam@dynamic:field(
                                <<"name"/utf8>>,
                                fun gleam@dynamic:string/1
                            ))(Dyn),
                            gleam@result:map_error(
                                _pipe@3,
                                fun to_schema_error/1
                            )
                        end,
                        fun(Name) ->
                            {ok, {named_type, Name, kind_from_string(Kind)}}
                        end
                    )
            end end
    ).

-file("src/squall/internal/schema.gleam", 247).
?DOC(false).
-spec decode_input_value(gleam@dynamic:dynamic_()) -> {ok, input_value()} |
    {error, squall@internal@error:error()}.
decode_input_value(Dyn) ->
    gleam@result:'try'(
        begin
            _pipe = (gleam@dynamic:field(
                <<"name"/utf8>>,
                fun gleam@dynamic:string/1
            ))(Dyn),
            gleam@result:map_error(_pipe, fun to_schema_error/1)
        end,
        fun(Name) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = (gleam@dynamic:field(
                        <<"type"/utf8>>,
                        fun gleam@dynamic:dynamic/1
                    ))(Dyn),
                    gleam@result:map_error(_pipe@1, fun to_schema_error/1)
                end,
                fun(Type_dyn) ->
                    gleam@result:'try'(
                        decode_type_ref(Type_dyn),
                        fun(Type_ref) ->
                            Description = begin
                                _pipe@2 = (gleam@dynamic:field(
                                    <<"description"/utf8>>,
                                    fun gleam@dynamic:string/1
                                ))(Dyn),
                                _pipe@3 = gleam@result:map(
                                    _pipe@2,
                                    fun(Field@0) -> {some, Field@0} end
                                ),
                                gleam@result:unwrap(_pipe@3, none)
                            end,
                            {ok, {input_value, Name, Type_ref, Description}}
                        end
                    )
                end
            )
        end
    ).

-file("src/squall/internal/schema.gleam", 220).
?DOC(false).
-spec decode_field(gleam@dynamic:dynamic_()) -> {ok, field()} |
    {error, squall@internal@error:error()}.
decode_field(Dyn) ->
    gleam@result:'try'(
        begin
            _pipe = (gleam@dynamic:field(
                <<"name"/utf8>>,
                fun gleam@dynamic:string/1
            ))(Dyn),
            gleam@result:map_error(_pipe, fun to_schema_error/1)
        end,
        fun(Name) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = (gleam@dynamic:field(
                        <<"type"/utf8>>,
                        fun gleam@dynamic:dynamic/1
                    ))(Dyn),
                    gleam@result:map_error(_pipe@1, fun to_schema_error/1)
                end,
                fun(Type_dyn) ->
                    gleam@result:'try'(
                        decode_type_ref(Type_dyn),
                        fun(Type_ref) ->
                            Args = case (gleam@dynamic:field(
                                <<"args"/utf8>>,
                                gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
                            ))(Dyn) of
                                {ok, Args_dyn} ->
                                    _pipe@2 = gleam@list:try_map(
                                        Args_dyn,
                                        fun decode_input_value/1
                                    ),
                                    gleam@result:unwrap(_pipe@2, []);

                                {error, _} ->
                                    []
                            end,
                            Description = begin
                                _pipe@3 = (gleam@dynamic:field(
                                    <<"description"/utf8>>,
                                    fun gleam@dynamic:string/1
                                ))(Dyn),
                                _pipe@4 = gleam@result:map(
                                    _pipe@3,
                                    fun(Field@0) -> {some, Field@0} end
                                ),
                                gleam@result:unwrap(_pipe@4, none)
                            end,
                            {ok, {field, Name, Type_ref, Args, Description}}
                        end
                    )
                end
            )
        end
    ).

-file("src/squall/internal/schema.gleam", 141).
?DOC(false).
-spec decode_type(gleam@dynamic:dynamic_()) -> {ok, type()} |
    {error, squall@internal@error:error()}.
decode_type(Dyn) ->
    gleam@result:'try'(
        begin
            _pipe = (gleam@dynamic:field(
                <<"name"/utf8>>,
                fun gleam@dynamic:string/1
            ))(Dyn),
            gleam@result:map_error(_pipe, fun to_schema_error/1)
        end,
        fun(Name) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = (gleam@dynamic:field(
                        <<"kind"/utf8>>,
                        fun gleam@dynamic:string/1
                    ))(Dyn),
                    gleam@result:map_error(_pipe@1, fun to_schema_error/1)
                end,
                fun(Kind) ->
                    Description = begin
                        _pipe@2 = (gleam@dynamic:field(
                            <<"description"/utf8>>,
                            fun gleam@dynamic:string/1
                        ))(Dyn),
                        _pipe@3 = gleam@result:map(
                            _pipe@2,
                            fun(Field@0) -> {some, Field@0} end
                        ),
                        gleam@result:unwrap(_pipe@3, none)
                    end,
                    case Kind of
                        <<"SCALAR"/utf8>> ->
                            {ok, {scalar_type, Name, Description}};

                        <<"OBJECT"/utf8>> ->
                            Fields = case (gleam@dynamic:field(
                                <<"fields"/utf8>>,
                                gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
                            ))(Dyn) of
                                {ok, Fields_dyn} ->
                                    _pipe@4 = gleam@list:try_map(
                                        Fields_dyn,
                                        fun decode_field/1
                                    ),
                                    gleam@result:unwrap(_pipe@4, []);

                                {error, _} ->
                                    []
                            end,
                            case Kind of
                                <<"OBJECT"/utf8>> ->
                                    {ok,
                                        {object_type, Name, Fields, Description}};

                                _ ->
                                    {ok,
                                        {interface_type,
                                            Name,
                                            Fields,
                                            Description}}
                            end;

                        <<"INTERFACE"/utf8>> ->
                            Fields = case (gleam@dynamic:field(
                                <<"fields"/utf8>>,
                                gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
                            ))(Dyn) of
                                {ok, Fields_dyn} ->
                                    _pipe@4 = gleam@list:try_map(
                                        Fields_dyn,
                                        fun decode_field/1
                                    ),
                                    gleam@result:unwrap(_pipe@4, []);

                                {error, _} ->
                                    []
                            end,
                            case Kind of
                                <<"OBJECT"/utf8>> ->
                                    {ok,
                                        {object_type, Name, Fields, Description}};

                                _ ->
                                    {ok,
                                        {interface_type,
                                            Name,
                                            Fields,
                                            Description}}
                            end;

                        <<"UNION"/utf8>> ->
                            Possible_types = case (gleam@dynamic:field(
                                <<"possibleTypes"/utf8>>,
                                gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
                            ))(Dyn) of
                                {ok, Types_dyn} ->
                                    _pipe@6 = gleam@list:try_map(
                                        Types_dyn,
                                        fun(D) ->
                                            _pipe@5 = (gleam@dynamic:field(
                                                <<"name"/utf8>>,
                                                fun gleam@dynamic:string/1
                                            ))(D),
                                            gleam@result:map_error(
                                                _pipe@5,
                                                fun to_schema_error/1
                                            )
                                        end
                                    ),
                                    gleam@result:unwrap(_pipe@6, []);

                                {error, _} ->
                                    []
                            end,
                            {ok,
                                {union_type, Name, Possible_types, Description}};

                        <<"ENUM"/utf8>> ->
                            Enum_values = case (gleam@dynamic:field(
                                <<"enumValues"/utf8>>,
                                gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
                            ))(Dyn) of
                                {ok, Values_dyn} ->
                                    _pipe@8 = gleam@list:try_map(
                                        Values_dyn,
                                        fun(D@1) ->
                                            _pipe@7 = (gleam@dynamic:field(
                                                <<"name"/utf8>>,
                                                fun gleam@dynamic:string/1
                                            ))(D@1),
                                            gleam@result:map_error(
                                                _pipe@7,
                                                fun to_schema_error/1
                                            )
                                        end
                                    ),
                                    gleam@result:unwrap(_pipe@8, []);

                                {error, _} ->
                                    []
                            end,
                            {ok, {enum_type, Name, Enum_values, Description}};

                        <<"INPUT_OBJECT"/utf8>> ->
                            Input_fields = case (gleam@dynamic:field(
                                <<"inputFields"/utf8>>,
                                gleam@dynamic:list(fun gleam@dynamic:dynamic/1)
                            ))(Dyn) of
                                {ok, Fields_dyn@1} ->
                                    _pipe@9 = gleam@list:try_map(
                                        Fields_dyn@1,
                                        fun decode_input_value/1
                                    ),
                                    gleam@result:unwrap(_pipe@9, []);

                                {error, _} ->
                                    []
                            end,
                            {ok,
                                {input_object_type,
                                    Name,
                                    Input_fields,
                                    Description}};

                        _ ->
                            {ok, {scalar_type, Name, Description}}
                    end
                end
            )
        end
    ).

-file("src/squall/internal/schema.gleam", 328).
?DOC(false).
-spec get_type_count(schema()) -> integer().
get_type_count(Schema) ->
    maps:size(erlang:element(5, Schema)).

-file("src/squall/internal/schema.gleam", 332).
?DOC(false).
-spec find_type(schema(), binary()) -> {ok, type()} |
    {error, squall@internal@error:error()}.
find_type(Schema, Name) ->
    _pipe = gleam@dict:get(erlang:element(5, Schema), Name),
    gleam@result:map_error(
        _pipe,
        fun(_) ->
            {invalid_schema_response, <<"Type not found: "/utf8, Name/binary>>}
        end
    ).

-file("src/squall/internal/schema.gleam", 339).
?DOC(false).
-spec get_type_name(type()) -> binary().
get_type_name(Type_) ->
    case Type_ of
        {scalar_type, Name, _} ->
            Name;

        {object_type, Name@1, _, _} ->
            Name@1;

        {interface_type, Name@2, _, _} ->
            Name@2;

        {union_type, Name@3, _, _} ->
            Name@3;

        {enum_type, Name@4, _, _} ->
            Name@4;

        {input_object_type, Name@5, _, _} ->
            Name@5
    end.

-file("src/squall/internal/schema.gleam", 83).
?DOC(false).
-spec decode_schema(gleam@dynamic:dynamic_()) -> {ok, schema()} |
    {error, squall@internal@error:error()}.
decode_schema(Dyn) ->
    gleam@result:'try'(
        begin
            _pipe = (gleam@dynamic:field(
                <<"data"/utf8>>,
                fun gleam@dynamic:dynamic/1
            ))(Dyn),
            gleam@result:map_error(_pipe, fun to_schema_error/1)
        end,
        fun(Data) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = (gleam@dynamic:field(
                        <<"__schema"/utf8>>,
                        fun gleam@dynamic:dynamic/1
                    ))(Data),
                    gleam@result:map_error(_pipe@1, fun to_schema_error/1)
                end,
                fun(Schema_obj) ->
                    Query_type = begin
                        _pipe@2 = (gleam@dynamic:field(
                            <<"queryType"/utf8>>,
                            fun gleam@dynamic:dynamic/1
                        ))(Schema_obj),
                        _pipe@3 = gleam@result:then(
                            _pipe@2,
                            gleam@dynamic:field(
                                <<"name"/utf8>>,
                                fun gleam@dynamic:string/1
                            )
                        ),
                        _pipe@4 = gleam@result:map(
                            _pipe@3,
                            fun(Field@0) -> {some, Field@0} end
                        ),
                        gleam@result:unwrap(_pipe@4, none)
                    end,
                    Mutation_type = begin
                        _pipe@5 = (gleam@dynamic:field(
                            <<"mutationType"/utf8>>,
                            fun gleam@dynamic:dynamic/1
                        ))(Schema_obj),
                        _pipe@6 = gleam@result:then(
                            _pipe@5,
                            gleam@dynamic:field(
                                <<"name"/utf8>>,
                                fun gleam@dynamic:string/1
                            )
                        ),
                        _pipe@7 = gleam@result:map(
                            _pipe@6,
                            fun(Field@0) -> {some, Field@0} end
                        ),
                        gleam@result:unwrap(_pipe@7, none)
                    end,
                    Subscription_type = begin
                        _pipe@8 = (gleam@dynamic:field(
                            <<"subscriptionType"/utf8>>,
                            fun gleam@dynamic:dynamic/1
                        ))(Schema_obj),
                        _pipe@9 = gleam@result:then(
                            _pipe@8,
                            gleam@dynamic:field(
                                <<"name"/utf8>>,
                                fun gleam@dynamic:string/1
                            )
                        ),
                        _pipe@10 = gleam@result:map(
                            _pipe@9,
                            fun(Field@0) -> {some, Field@0} end
                        ),
                        gleam@result:unwrap(_pipe@10, none)
                    end,
                    gleam@result:'try'(
                        begin
                            _pipe@11 = (gleam@dynamic:field(
                                <<"types"/utf8>>,
                                fun gleam@dynamic:dynamic/1
                            ))(Schema_obj),
                            gleam@result:map_error(
                                _pipe@11,
                                fun to_schema_error/1
                            )
                        end,
                        fun(Types_dynamic) ->
                            gleam@result:'try'(
                                begin
                                    _pipe@12 = (gleam@dynamic:list(
                                        fun gleam@dynamic:dynamic/1
                                    ))(Types_dynamic),
                                    gleam@result:map_error(
                                        _pipe@12,
                                        fun to_schema_error/1
                                    )
                                end,
                                fun(Types_list_dyn) ->
                                    Types_result = begin
                                        _pipe@13 = Types_list_dyn,
                                        gleam@list:try_map(
                                            _pipe@13,
                                            fun decode_type/1
                                        )
                                    end,
                                    gleam@result:'try'(
                                        Types_result,
                                        fun(Types_list) ->
                                            Types = begin
                                                _pipe@14 = Types_list,
                                                _pipe@15 = gleam@list:map(
                                                    _pipe@14,
                                                    fun(T) ->
                                                        {get_type_name(T), T}
                                                    end
                                                ),
                                                maps:from_list(_pipe@15)
                                            end,
                                            {ok,
                                                {schema,
                                                    Query_type,
                                                    Mutation_type,
                                                    Subscription_type,
                                                    Types}}
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

-file("src/squall/internal/schema.gleam", 76).
?DOC(false).
-spec parse_introspection_response(binary()) -> {ok, schema()} |
    {error, squall@internal@error:error()}.
parse_introspection_response(Json_str) ->
    case gleam@json:decode(Json_str, fun gleam@dynamic:dynamic/1) of
        {ok, Dyn} ->
            decode_schema(Dyn);

        {error, _} ->
            {error, {invalid_schema_response, <<"Invalid JSON"/utf8>>}}
    end.

-file("src/squall/internal/schema.gleam", 350).
?DOC(false).
-spec get_type_fields(type()) -> list(field()).
get_type_fields(Type_) ->
    case Type_ of
        {object_type, _, Fields, _} ->
            Fields;

        {interface_type, _, Fields@1, _} ->
            Fields@1;

        _ ->
            []
    end.

-file("src/squall/internal/schema.gleam", 358).
?DOC(false).
-spec get_query_type_name(schema()) -> {ok, binary()} |
    {error, squall@internal@error:error()}.
get_query_type_name(Schema) ->
    case erlang:element(2, Schema) of
        {some, Name} ->
            {ok, Name};

        none ->
            {error, {invalid_schema_response, <<"No query type defined"/utf8>>}}
    end.
