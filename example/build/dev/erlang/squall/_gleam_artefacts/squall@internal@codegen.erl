-module(squall@internal@codegen).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/squall/internal/codegen.gleam").
-export([generate_operation/4]).
-export_type([generated_code/0, nested_type_info/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type generated_code() :: {generated_code,
        binary(),
        list(binary()),
        list(binary())}.

-type nested_type_info() :: {nested_type_info,
        binary(),
        list({binary(), squall@internal@schema:type_ref()}),
        gleam@dict:dict(binary(), squall@internal@schema:type())}.

-file("src/squall/internal/codegen.gleam", 85).
?DOC(false).
-spec let_var(binary(), glam@doc:document()) -> glam@doc:document().
let_var(Name, Body) ->
    _pipe = [glam@doc:from_string(
            <<<<"let "/utf8, Name/binary>>/binary, " ="/utf8>>
        ),
        {break, <<" "/utf8>>, <<""/utf8>>},
        Body],
    _pipe@1 = glam@doc:concat(_pipe),
    glam@doc:group(_pipe@1).

-file("src/squall/internal/codegen.gleam", 92).
?DOC(false).
-spec string_doc(binary()) -> glam@doc:document().
string_doc(Content) ->
    Escaped_string = begin
        _pipe = Content,
        _pipe@1 = gleam@string:replace(_pipe, <<"\\"/utf8>>, <<"\\\\"/utf8>>),
        _pipe@2 = gleam@string:replace(_pipe@1, <<"\""/utf8>>, <<"\\\""/utf8>>),
        _pipe@3 = gleam@string:replace(_pipe@2, <<"\n"/utf8>>, <<"\\n"/utf8>>),
        glam@doc:from_string(_pipe@3)
    end,
    _pipe@4 = [glam@doc:from_string(<<"\""/utf8>>),
        Escaped_string,
        glam@doc:from_string(<<"\""/utf8>>)],
    glam@doc:concat(_pipe@4).

-file("src/squall/internal/codegen.gleam", 114).
?DOC(false).
-spec imports_doc() -> glam@doc:document().
imports_doc() ->
    Import_lines = [<<"import gleam/dynamic"/utf8>>,
        <<"import gleam/http"/utf8>>,
        <<"import gleam/http/request"/utf8>>,
        <<"import gleam/httpc"/utf8>>,
        <<"import gleam/json"/utf8>>,
        <<"import gleam/option.{type Option}"/utf8>>,
        <<"import gleam/result"/utf8>>],
    _pipe = Import_lines,
    _pipe@1 = gleam@list:map(_pipe, fun glam@doc:from_string/1),
    glam@doc:join(_pipe@1, {line, 1}).

-file("src/squall/internal/codegen.gleam", 224).
?DOC(false).
-spec collect_field_types(
    list(squall@internal@parser:selection()),
    squall@internal@schema:type()
) -> {ok, list({binary(), squall@internal@schema:type_ref()})} |
    {error, squall@internal@error:error()}.
collect_field_types(Selections, Parent_type) ->
    _pipe = Selections,
    gleam@list:try_map(_pipe, fun(Selection) -> case Selection of
                {field_selection, Field_name, _, _, _} ->
                    Fields = squall@internal@schema:get_type_fields(Parent_type),
                    Field_result = begin
                        _pipe@1 = Fields,
                        gleam@list:find(
                            _pipe@1,
                            fun(F) -> erlang:element(2, F) =:= Field_name end
                        )
                    end,
                    gleam@result:'try'(
                        begin
                            _pipe@2 = Field_result,
                            gleam@result:map_error(
                                _pipe@2,
                                fun(_) ->
                                    {invalid_schema_response,
                                        <<"Field not found: "/utf8,
                                            Field_name/binary>>}
                                end
                            )
                        end,
                        fun(Field) ->
                            {ok, {Field_name, erlang:element(3, Field)}}
                        end
                    )
            end end).

-file("src/squall/internal/codegen.gleam", 316).
?DOC(false).
-spec get_base_type_name(squall@internal@schema:type_ref()) -> binary().
get_base_type_name(Type_ref) ->
    case Type_ref of
        {named_type, Name, _} ->
            Name;

        {non_null_type, Inner} ->
            get_base_type_name(Inner);

        {list_type, Inner@1} ->
            get_base_type_name(Inner@1)
    end.

-file("src/squall/internal/codegen.gleam", 252).
?DOC(false).
-spec collect_nested_types(
    list(squall@internal@parser:selection()),
    squall@internal@schema:type(),
    squall@internal@schema:schema()
) -> {ok, list(nested_type_info())} | {error, squall@internal@error:error()}.
collect_nested_types(Selections, Parent_type, Schema_data) ->
    _pipe = Selections,
    _pipe@4 = gleam@list:try_map(_pipe, fun(Selection) -> case Selection of
                {field_selection, Field_name, _, _, Nested_selections} ->
                    Fields = squall@internal@schema:get_type_fields(Parent_type),
                    gleam@result:'try'(
                        begin
                            _pipe@1 = Fields,
                            _pipe@2 = gleam@list:find(
                                _pipe@1,
                                fun(F) ->
                                    erlang:element(2, F) =:= Field_name
                                end
                            ),
                            gleam@result:map_error(
                                _pipe@2,
                                fun(_) ->
                                    {invalid_schema_response,
                                        <<"Field not found: "/utf8,
                                            Field_name/binary>>}
                                end
                            )
                        end,
                        fun(Field) -> case Nested_selections of
                                [] ->
                                    {ok, []};

                                _ ->
                                    Type_name = get_base_type_name(
                                        erlang:element(3, Field)
                                    ),
                                    gleam@result:'try'(
                                        begin
                                            _pipe@3 = gleam@dict:get(
                                                erlang:element(5, Schema_data),
                                                Type_name
                                            ),
                                            gleam@result:map_error(
                                                _pipe@3,
                                                fun(_) ->
                                                    {invalid_schema_response,
                                                        <<"Type not found: "/utf8,
                                                            Type_name/binary>>}
                                                end
                                            )
                                        end,
                                        fun(Field_type) ->
                                            gleam@result:'try'(
                                                collect_field_types(
                                                    Nested_selections,
                                                    Field_type
                                                ),
                                                fun(Nested_field_types) ->
                                                    gleam@result:'try'(
                                                        collect_nested_types(
                                                            Nested_selections,
                                                            Field_type,
                                                            Schema_data
                                                        ),
                                                        fun(Deeper_nested) ->
                                                            Nested_info = {nested_type_info,
                                                                Type_name,
                                                                Nested_field_types,
                                                                erlang:element(
                                                                    5,
                                                                    Schema_data
                                                                )},
                                                            {ok,
                                                                [Nested_info |
                                                                    Deeper_nested]}
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                            end end
                    )
            end end),
    gleam@result:map(_pipe@4, fun gleam@list:flatten/1).

-file("src/squall/internal/codegen.gleam", 483).
?DOC(false).
-spec generate_field_decoder(squall@internal@type_mapping:gleam_type()) -> binary().
generate_field_decoder(Gleam_type) ->
    case Gleam_type of
        string_type ->
            <<"dynamic.string"/utf8>>;

        int_type ->
            <<"dynamic.int"/utf8>>;

        float_type ->
            <<"dynamic.float"/utf8>>;

        bool_type ->
            <<"dynamic.bool"/utf8>>;

        {list_type, Inner} ->
            <<<<"dynamic.list("/utf8, (generate_field_decoder(Inner))/binary>>/binary,
                ")"/utf8>>;

        {option_type, Inner@1} ->
            <<<<"dynamic.optional("/utf8,
                    (generate_field_decoder(Inner@1))/binary>>/binary,
                ")"/utf8>>;

        {custom_type, _} ->
            <<"dynamic.dynamic"/utf8>>
    end.

-file("src/squall/internal/codegen.gleam", 750).
?DOC(false).
-spec type_ref_to_string(squall@internal@parser:type_ref()) -> binary().
type_ref_to_string(Type_ref) ->
    case Type_ref of
        {named_type_ref, Name} ->
            Name;

        {list_type_ref, Inner} ->
            <<<<"["/utf8, (type_ref_to_string(Inner))/binary>>/binary,
                "]"/utf8>>;

        {non_null_type_ref, Inner@1} ->
            <<(type_ref_to_string(Inner@1))/binary, "!"/utf8>>
    end.

-file("src/squall/internal/codegen.gleam", 760).
?DOC(false).
-spec capitalize(binary()) -> binary().
capitalize(S) ->
    case gleam@string:pop_grapheme(S) of
        {ok, {First, Rest}} ->
            <<(gleam@string:uppercase(First))/binary, Rest/binary>>;

        {error, _} ->
            S
    end.

-file("src/squall/internal/codegen.gleam", 767).
?DOC(false).
-spec to_pascal_case(binary()) -> binary().
to_pascal_case(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:split(_pipe, <<"_"/utf8>>),
    _pipe@2 = gleam@list:map(_pipe@1, fun capitalize/1),
    gleam@string:join(_pipe@2, <<""/utf8>>).

-file("src/squall/internal/codegen.gleam", 774).
?DOC(false).
-spec format_value(squall@internal@parser:value()) -> binary().
format_value(Value) ->
    case Value of
        {int_value, I} ->
            gleam@int:to_string(I);

        {float_value, F} ->
            gleam@float:to_string(F);

        {string_value, S} ->
            <<<<"\""/utf8, S/binary>>/binary, "\""/utf8>>;

        {boolean_value, true} ->
            <<"true"/utf8>>;

        {boolean_value, false} ->
            <<"false"/utf8>>;

        null_value ->
            <<"null"/utf8>>;

        {variable_value, Name} ->
            <<"$"/utf8, Name/binary>>;

        {list_value, Values} ->
            Formatted_values = begin
                _pipe = Values,
                _pipe@1 = gleam@list:map(_pipe, fun format_value/1),
                gleam@string:join(_pipe@1, <<", "/utf8>>)
            end,
            <<<<"["/utf8, Formatted_values/binary>>/binary, "]"/utf8>>;

        {object_value, Fields} ->
            Formatted_fields = begin
                _pipe@2 = Fields,
                _pipe@3 = gleam@list:map(
                    _pipe@2,
                    fun(Field) ->
                        {Name@1, Value@1} = Field,
                        <<<<Name@1/binary, ": "/utf8>>/binary,
                            (format_value(Value@1))/binary>>
                    end
                ),
                gleam@string:join(_pipe@3, <<", "/utf8>>)
            end,
            <<<<"{ "/utf8, Formatted_fields/binary>>/binary, " }"/utf8>>
    end.

-file("src/squall/internal/codegen.gleam", 803).
?DOC(false).
-spec format_arguments(list(squall@internal@parser:argument())) -> binary().
format_arguments(Arguments) ->
    case Arguments of
        [] ->
            <<""/utf8>>;

        Args ->
            Formatted_args = begin
                _pipe = Args,
                _pipe@1 = gleam@list:map(
                    _pipe,
                    fun(Arg) ->
                        {argument, Name, Value} = Arg,
                        <<<<Name/binary, ": "/utf8>>/binary,
                            (format_value(Value))/binary>>
                    end
                ),
                gleam@string:join(_pipe@1, <<", "/utf8>>)
            end,
            <<<<"("/utf8, Formatted_args/binary>>/binary, ")"/utf8>>
    end.

-file("src/squall/internal/codegen.gleam", 731).
?DOC(false).
-spec build_selection_set(list(squall@internal@parser:selection())) -> binary().
build_selection_set(Selections) ->
    Fields = begin
        _pipe = Selections,
        _pipe@1 = gleam@list:map(_pipe, fun(Selection) -> case Selection of
                    {field_selection, Name, _, Args, Nested} ->
                        Args_str = format_arguments(Args),
                        case Nested of
                            [] ->
                                <<Name/binary, Args_str/binary>>;

                            Subs ->
                                <<<<<<Name/binary, Args_str/binary>>/binary,
                                        " "/utf8>>/binary,
                                    (build_selection_set(Subs))/binary>>
                        end
                end end),
        gleam@string:join(_pipe@1, <<" "/utf8>>)
    end,
    <<<<"{ "/utf8, Fields/binary>>/binary, " }"/utf8>>.

-file("src/squall/internal/codegen.gleam", 699).
?DOC(false).
-spec build_query_string(squall@internal@parser:operation()) -> binary().
build_query_string(Operation) ->
    Op_type = case squall@internal@parser:get_operation_type(Operation) of
        'query' ->
            <<"query"/utf8>>;

        mutation ->
            <<"mutation"/utf8>>;

        subscription ->
            <<"subscription"/utf8>>
    end,
    Op_name = case squall@internal@parser:get_operation_name(Operation) of
        {some, Name} ->
            <<" "/utf8, Name/binary>>;

        none ->
            <<""/utf8>>
    end,
    Variables = squall@internal@parser:get_variables(Operation),
    Var_defs = case Variables of
        [] ->
            <<""/utf8>>;

        Vars ->
            Defs = begin
                _pipe = Vars,
                _pipe@1 = gleam@list:map(
                    _pipe,
                    fun(Var) ->
                        <<<<<<"$"/utf8, (erlang:element(2, Var))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (type_ref_to_string(erlang:element(3, Var)))/binary>>
                    end
                ),
                gleam@string:join(_pipe@1, <<", "/utf8>>)
            end,
            <<<<"("/utf8, Defs/binary>>/binary, ")"/utf8>>
    end,
    Selections = squall@internal@parser:get_selections(Operation),
    Selection_set = build_selection_set(Selections),
    <<<<<<<<Op_type/binary, Op_name/binary>>/binary, Var_defs/binary>>/binary,
            " "/utf8>>/binary,
        Selection_set/binary>>.

-file("src/squall/internal/codegen.gleam", 819).
?DOC(false).
-spec snake_case(binary()) -> binary().
snake_case(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    gleam@list:fold(
        _pipe@1,
        <<""/utf8>>,
        fun(Acc, Char) ->
            case (gleam@string:uppercase(Char) =:= Char) andalso (Char /= gleam@string:lowercase(
                Char
            )) of
                true ->
                    case Acc of
                        <<""/utf8>> ->
                            gleam@string:lowercase(Char);

                        _ ->
                            <<<<Acc/binary, "_"/utf8>>/binary,
                                (gleam@string:lowercase(Char))/binary>>
                    end;

                false ->
                    <<Acc/binary, Char/binary>>
            end
        end
    ).

-file("src/squall/internal/codegen.gleam", 454).
?DOC(false).
-spec generate_field_decoder_with_schema_inner(
    squall@internal@type_mapping:gleam_type(),
    squall@internal@schema:type_ref(),
    gleam@dict:dict(binary(), squall@internal@schema:type())
) -> binary().
generate_field_decoder_with_schema_inner(Gleam_type, Type_ref, Schema_types) ->
    Base_type_name = get_base_type_name(Type_ref),
    case Gleam_type of
        {list_type, Inner} ->
            Inner_decoder = generate_field_decoder_with_schema_inner(
                Inner,
                Type_ref,
                Schema_types
            ),
            <<<<"dynamic.list("/utf8, Inner_decoder/binary>>/binary, ")"/utf8>>;

        {option_type, Inner@1} ->
            Inner_decoder@1 = generate_field_decoder_with_schema_inner(
                Inner@1,
                Type_ref,
                Schema_types
            ),
            <<<<"dynamic.optional("/utf8, Inner_decoder@1/binary>>/binary,
                ")"/utf8>>;

        {custom_type, _} ->
            case gleam@dict:get(Schema_types, Base_type_name) of
                {ok, {object_type, _, _, _}} ->
                    <<(snake_case(Base_type_name))/binary, "_decoder()"/utf8>>;

                _ ->
                    <<"dynamic.dynamic"/utf8>>
            end;

        _ ->
            generate_field_decoder(Gleam_type)
    end.

-file("src/squall/internal/codegen.gleam", 422).
?DOC(false).
-spec generate_field_decoder_with_schema(
    squall@internal@type_mapping:gleam_type(),
    squall@internal@schema:type_ref(),
    gleam@dict:dict(binary(), squall@internal@schema:type())
) -> binary().
generate_field_decoder_with_schema(Gleam_type, Type_ref, Schema_types) ->
    case Gleam_type of
        string_type ->
            <<"dynamic.string"/utf8>>;

        int_type ->
            <<"dynamic.int"/utf8>>;

        float_type ->
            <<"dynamic.float"/utf8>>;

        bool_type ->
            <<"dynamic.bool"/utf8>>;

        {list_type, Inner} ->
            Inner_decoder = generate_field_decoder_with_schema_inner(
                Inner,
                Type_ref,
                Schema_types
            ),
            <<<<"dynamic.list("/utf8, Inner_decoder/binary>>/binary, ")"/utf8>>;

        {option_type, Inner@1} ->
            Inner_decoder@1 = generate_field_decoder_with_schema_inner(
                Inner@1,
                Type_ref,
                Schema_types
            ),
            <<<<"dynamic.optional("/utf8, Inner_decoder@1/binary>>/binary,
                ")"/utf8>>;

        {custom_type, Name} ->
            case gleam@dict:get(Schema_types, Name) of
                {ok, {object_type, _, _, _}} ->
                    <<(snake_case(Name))/binary, "_decoder()"/utf8>>;

                _ ->
                    <<"dynamic.dynamic"/utf8>>
            end
    end.

-file("src/squall/internal/codegen.gleam", 51).
?DOC(false).
-spec comma_list(binary(), list(glam@doc:document()), binary()) -> glam@doc:document().
comma_list(Open, Content, Close) ->
    case Content of
        [] ->
            glam@doc:from_string(<<Open/binary, Close/binary>>);

        _ ->
            _pipe@2 = [glam@doc:from_string(Open),
                begin
                    _pipe = [{break, <<""/utf8>>, <<""/utf8>>},
                        glam@doc:join(
                            Content,
                            glam@doc:break(<<", "/utf8>>, <<","/utf8>>)
                        )],
                    _pipe@1 = glam@doc:concat(_pipe),
                    glam@doc:nest(_pipe@1, 2)
                end,
                glam@doc:break(<<""/utf8>>, <<","/utf8>>),
                glam@doc:from_string(Close)],
            glam@doc:concat(_pipe@2)
    end.

-file("src/squall/internal/codegen.gleam", 44).
?DOC(false).
-spec call_doc(binary(), list(glam@doc:document())) -> glam@doc:document().
call_doc(Function, Args) ->
    _pipe@1 = [glam@doc:from_string(Function),
        begin
            _pipe = comma_list(<<"("/utf8>>, Args, <<")"/utf8>>),
            glam@doc:group(_pipe)
        end],
    _pipe@2 = glam@doc:concat(_pipe@1),
    glam@doc:group(_pipe@2).

-file("src/squall/internal/codegen.gleam", 71).
?DOC(false).
-spec block(list(glam@doc:document())) -> glam@doc:document().
block(Body) ->
    _pipe@3 = [glam@doc:from_string(<<"{"/utf8>>),
        begin
            _pipe = {line, 1},
            glam@doc:nest(_pipe, 2)
        end,
        begin
            _pipe@1 = Body,
            _pipe@2 = glam@doc:join(_pipe@1, {line, 1}),
            glam@doc:nest(_pipe@2, 2)
        end,
        {line, 1},
        glam@doc:from_string(<<"}"/utf8>>)],
    glam@doc:concat(_pipe@3).

-file("src/squall/internal/codegen.gleam", 665).
?DOC(false).
-spec encode_variable_value(binary(), squall@internal@type_mapping:gleam_type()) -> glam@doc:document().
encode_variable_value(Var_name, Gleam_type) ->
    case Gleam_type of
        string_type ->
            call_doc(<<"json.string"/utf8>>, [glam@doc:from_string(Var_name)]);

        int_type ->
            call_doc(<<"json.int"/utf8>>, [glam@doc:from_string(Var_name)]);

        float_type ->
            call_doc(<<"json.float"/utf8>>, [glam@doc:from_string(Var_name)]);

        bool_type ->
            call_doc(<<"json.bool"/utf8>>, [glam@doc:from_string(Var_name)]);

        {list_type, Inner} ->
            Encoder = case Inner of
                string_type ->
                    <<"json.string"/utf8>>;

                int_type ->
                    <<"json.int"/utf8>>;

                float_type ->
                    <<"json.float"/utf8>>;

                bool_type ->
                    <<"json.bool"/utf8>>;

                _ ->
                    <<"json.string"/utf8>>
            end,
            call_doc(
                <<"json.array"/utf8>>,
                [glam@doc:from_string(<<"from: "/utf8, Var_name/binary>>),
                    glam@doc:from_string(<<"of: "/utf8, Encoder/binary>>)]
            );

        {option_type, Inner@1} ->
            call_doc(
                <<"json.nullable"/utf8>>,
                [glam@doc:from_string(Var_name),
                    encode_variable_value(<<"value"/utf8>>, Inner@1)]
            );

        {custom_type, _} ->
            call_doc(<<"json.string"/utf8>>, [glam@doc:from_string(Var_name)])
    end.

-file("src/squall/internal/codegen.gleam", 498).
?DOC(false).
-spec generate_function(
    binary(),
    binary(),
    list(squall@internal@parser:variable()),
    binary()
) -> glam@doc:document().
generate_function(Operation_name, Response_type_name, Variables, Query_string) ->
    Function_name = Operation_name,
    Param_docs = case Variables of
        [] ->
            [glam@doc:from_string(<<"endpoint: String"/utf8>>)];

        Vars ->
            Var_param_docs = begin
                _pipe = Vars,
                _pipe@2 = gleam@list:map(
                    _pipe,
                    fun(Var) ->
                        gleam@result:'try'(
                            begin
                                _pipe@1 = squall@internal@type_mapping:parser_type_to_schema_type(
                                    erlang:element(3, Var)
                                ),
                                gleam@result:then(
                                    _pipe@1,
                                    fun squall@internal@type_mapping:graphql_to_gleam/1
                                )
                            end,
                            fun(Gleam_type) ->
                                Param_name = snake_case(erlang:element(2, Var)),
                                {ok,
                                    glam@doc:from_string(
                                        <<<<Param_name/binary, ": "/utf8>>/binary,
                                            (squall@internal@type_mapping:to_gleam_type_string(
                                                Gleam_type
                                            ))/binary>>
                                    )}
                            end
                        )
                    end
                ),
                gleam@list:filter_map(_pipe@2, fun(R) -> R end)
            end,
            [glam@doc:from_string(<<"endpoint: String"/utf8>>) | Var_param_docs]
    end,
    Variables_code = case Variables of
        [] ->
            call_doc(
                <<"json.object"/utf8>>,
                [glam@doc:from_string(<<"[]"/utf8>>)]
            );

        Vars@1 ->
            Var_entry_docs = begin
                _pipe@3 = Vars@1,
                _pipe@5 = gleam@list:map(
                    _pipe@3,
                    fun(Var@1) ->
                        gleam@result:'try'(
                            begin
                                _pipe@4 = squall@internal@type_mapping:parser_type_to_schema_type(
                                    erlang:element(3, Var@1)
                                ),
                                gleam@result:then(
                                    _pipe@4,
                                    fun squall@internal@type_mapping:graphql_to_gleam/1
                                )
                            end,
                            fun(Gleam_type@1) ->
                                Param_name@1 = snake_case(
                                    erlang:element(2, Var@1)
                                ),
                                Value_encoder = encode_variable_value(
                                    Param_name@1,
                                    Gleam_type@1
                                ),
                                {ok,
                                    glam@doc:concat(
                                        [glam@doc:from_string(<<"#("/utf8>>),
                                            string_doc(erlang:element(2, Var@1)),
                                            glam@doc:from_string(<<", "/utf8>>),
                                            Value_encoder,
                                            glam@doc:from_string(<<")"/utf8>>)]
                                    )}
                            end
                        )
                    end
                ),
                gleam@list:filter_map(_pipe@5, fun(R@1) -> R@1 end)
            end,
            call_doc(
                <<"json.object"/utf8>>,
                [comma_list(<<"["/utf8>>, Var_entry_docs, <<"]"/utf8>>)]
            )
    end,
    Body_docs = [let_var(<<"query"/utf8>>, string_doc(Query_string)),
        let_var(<<"variables"/utf8>>, Variables_code),
        let_var(
            <<"body"/utf8>>,
            call_doc(
                <<"json.object"/utf8>>,
                [comma_list(
                        <<"["/utf8>>,
                        [glam@doc:from_string(
                                <<"#(\"query\", json.string(query))"/utf8>>
                            ),
                            glam@doc:from_string(
                                <<"#(\"variables\", variables)"/utf8>>
                            )],
                        <<"]"/utf8>>
                    )]
            )
        ),
        glam@doc:concat(
            [glam@doc:from_string(<<"use req <- result.try("/utf8>>),
                begin
                    _pipe@6 = glam@doc:concat(
                        [{line, 1},
                            call_doc(
                                <<"request.to"/utf8>>,
                                [glam@doc:from_string(<<"endpoint"/utf8>>)]
                            ),
                            {line, 1},
                            glam@doc:from_string(
                                <<"|> result.map_error(fn(_) { \"Invalid endpoint URL\" }),"/utf8>>
                            )]
                    ),
                    glam@doc:nest(_pipe@6, 2)
                end,
                {line, 1},
                glam@doc:from_string(<<")"/utf8>>)]
        ),
        let_var(
            <<"req"/utf8>>,
            glam@doc:concat(
                [glam@doc:from_string(<<"req"/utf8>>),
                    {line, 1},
                    glam@doc:from_string(
                        <<"|> request.set_method(http.Post)"/utf8>>
                    ),
                    {line, 1},
                    glam@doc:from_string(
                        <<"|> request.set_body(json.to_string(body))"/utf8>>
                    ),
                    {line, 1},
                    glam@doc:from_string(
                        <<"|> request.set_header(\"content-type\", \"application/json\")"/utf8>>
                    )]
            )
        ),
        glam@doc:concat(
            [glam@doc:from_string(<<"use resp <- result.try("/utf8>>),
                begin
                    _pipe@7 = glam@doc:concat(
                        [{line, 1},
                            call_doc(
                                <<"httpc.send"/utf8>>,
                                [glam@doc:from_string(<<"req"/utf8>>)]
                            ),
                            {line, 1},
                            glam@doc:from_string(
                                <<"|> result.map_error(fn(_) { \"HTTP request failed\" }),"/utf8>>
                            )]
                    ),
                    glam@doc:nest(_pipe@7, 2)
                end,
                {line, 1},
                glam@doc:from_string(<<")"/utf8>>)]
        ),
        glam@doc:concat(
            [glam@doc:from_string(<<"use json_value <- result.try("/utf8>>),
                begin
                    _pipe@8 = glam@doc:concat(
                        [{line, 1},
                            call_doc(
                                <<"json.decode"/utf8>>,
                                [glam@doc:from_string(
                                        <<"from: resp.body"/utf8>>
                                    ),
                                    glam@doc:from_string(
                                        <<"using: dynamic.dynamic"/utf8>>
                                    )]
                            ),
                            {line, 1},
                            glam@doc:from_string(
                                <<"|> result.map_error(fn(_) { \"Failed to decode JSON response\" }),"/utf8>>
                            )]
                    ),
                    glam@doc:nest(_pipe@8, 2)
                end,
                {line, 1},
                glam@doc:from_string(<<")"/utf8>>)]
        ),
        glam@doc:concat(
            [glam@doc:from_string(<<"use data_field <- result.try("/utf8>>),
                begin
                    _pipe@9 = glam@doc:concat(
                        [{line, 1},
                            call_doc(
                                <<"dynamic.field"/utf8>>,
                                [string_doc(<<"data"/utf8>>),
                                    glam@doc:from_string(
                                        <<"dynamic.dynamic"/utf8>>
                                    )]
                            ),
                            glam@doc:from_string(<<"(json_value)"/utf8>>),
                            {line, 1},
                            glam@doc:from_string(
                                <<"|> result.map_error(fn(_) { \"No data field in response\" }),"/utf8>>
                            )]
                    ),
                    glam@doc:nest(_pipe@9, 2)
                end,
                {line, 1},
                glam@doc:from_string(<<")"/utf8>>)]
        ),
        glam@doc:concat(
            [glam@doc:from_string(
                    <<(snake_case(Response_type_name))/binary,
                        "_decoder()(data_field)"/utf8>>
                ),
                {line, 1},
                glam@doc:from_string(
                    <<"|> result.map_error(fn(_) { \"Failed to decode response data\" })"/utf8>>
                )]
        )],
    Return_type = call_doc(
        <<"Result"/utf8>>,
        [glam@doc:from_string(Response_type_name),
            glam@doc:from_string(<<"String"/utf8>>)]
    ),
    glam@doc:concat(
        [glam@doc:from_string(<<"pub fn "/utf8, Function_name/binary>>),
            comma_list(<<"("/utf8>>, Param_docs, <<")"/utf8>>),
            glam@doc:from_string(<<" -> "/utf8>>),
            Return_type,
            glam@doc:from_string(<<" "/utf8>>),
            block(Body_docs)]
    ).

-file("src/squall/internal/codegen.gleam", 105).
?DOC(false).
-spec sanitize_field_name(binary()) -> binary().
sanitize_field_name(Name) ->
    Snake_cased = snake_case(Name),
    case gleam@list:contains(
        [<<"as"/utf8>>,
            <<"assert"/utf8>>,
            <<"case"/utf8>>,
            <<"const"/utf8>>,
            <<"external"/utf8>>,
            <<"fn"/utf8>>,
            <<"if"/utf8>>,
            <<"import"/utf8>>,
            <<"let"/utf8>>,
            <<"opaque"/utf8>>,
            <<"pub"/utf8>>,
            <<"todo"/utf8>>,
            <<"try"/utf8>>,
            <<"type"/utf8>>,
            <<"use"/utf8>>],
        Snake_cased
    ) of
        true ->
            <<Snake_cased/binary, "_"/utf8>>;

        false ->
            Snake_cased
    end.

-file("src/squall/internal/codegen.gleam", 325).
?DOC(false).
-spec generate_type_definition(
    binary(),
    list({binary(), squall@internal@schema:type_ref()})
) -> glam@doc:document().
generate_type_definition(Type_name, Fields) ->
    Field_docs = begin
        _pipe = Fields,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Field) ->
                {Name, Type_ref} = Field,
                Sanitized_name = sanitize_field_name(Name),
                gleam@result:'try'(
                    squall@internal@type_mapping:graphql_to_gleam_nullable(
                        Type_ref
                    ),
                    fun(Gleam_type) ->
                        Field_doc = glam@doc:concat(
                            [glam@doc:from_string(
                                    <<Sanitized_name/binary, ": "/utf8>>
                                ),
                                glam@doc:from_string(
                                    squall@internal@type_mapping:to_gleam_type_string(
                                        Gleam_type
                                    )
                                )]
                        ),
                        {ok, Field_doc}
                    end
                )
            end
        ),
        gleam@list:filter_map(_pipe@1, fun(R) -> R end)
    end,
    _pipe@4 = [glam@doc:from_string(
            <<<<"pub type "/utf8, Type_name/binary>>/binary, " {"/utf8>>
        ),
        begin
            _pipe@2 = [{line, 1}, call_doc(Type_name, Field_docs)],
            _pipe@3 = glam@doc:concat(_pipe@2),
            glam@doc:nest(_pipe@3, 2)
        end,
        {line, 1},
        glam@doc:from_string(<<"}"/utf8>>)],
    _pipe@5 = glam@doc:concat(_pipe@4),
    glam@doc:group(_pipe@5).

-file("src/squall/internal/codegen.gleam", 362).
?DOC(false).
-spec generate_decoder_with_schema(
    binary(),
    list({binary(), squall@internal@schema:type_ref()}),
    gleam@dict:dict(binary(), squall@internal@schema:type())
) -> glam@doc:document().
generate_decoder_with_schema(Type_name, Fields, Schema_types) ->
    Decoder_name = <<(snake_case(Type_name))/binary, "_decoder"/utf8>>,
    Field_decoder_docs = begin
        _pipe = Fields,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Field) ->
                {Name, Type_ref} = Field,
                Sanitized_name = sanitize_field_name(Name),
                gleam@result:'try'(
                    squall@internal@type_mapping:graphql_to_gleam_nullable(
                        Type_ref
                    ),
                    fun(Gleam_type) ->
                        Decoder_call = call_doc(
                            <<"dynamic.field"/utf8>>,
                            [string_doc(Name),
                                glam@doc:from_string(
                                    generate_field_decoder_with_schema(
                                        Gleam_type,
                                        Type_ref,
                                        Schema_types
                                    )
                                )]
                        ),
                        Field_decoder = glam@doc:concat(
                            [glam@doc:from_string(
                                    <<<<"use "/utf8, Sanitized_name/binary>>/binary,
                                        " <- result.try("/utf8>>
                                ),
                                Decoder_call,
                                glam@doc:from_string(<<"(data))"/utf8>>)]
                        ),
                        {ok, Field_decoder}
                    end
                )
            end
        ),
        gleam@list:filter_map(_pipe@1, fun(R) -> R end)
    end,
    Constructor_args = begin
        _pipe@2 = Fields,
        gleam@list:map(
            _pipe@2,
            fun(F) ->
                Sanitized = sanitize_field_name(erlang:element(1, F)),
                glam@doc:from_string(
                    <<<<Sanitized/binary, ": "/utf8>>/binary, Sanitized/binary>>
                )
            end
        )
    end,
    Success_line = call_doc(
        <<"Ok"/utf8>>,
        [call_doc(Type_name, Constructor_args)]
    ),
    Inner_fn_body = lists:append(Field_decoder_docs, [Success_line]),
    Inner_fn = glam@doc:concat(
        [glam@doc:from_string(<<"fn(data: dynamic.Dynamic) -> Result("/utf8>>),
            glam@doc:from_string(
                <<Type_name/binary, ", List(dynamic.DecodeError)) "/utf8>>
            ),
            block(Inner_fn_body)]
    ),
    glam@doc:concat(
        [glam@doc:from_string(
                <<<<"pub fn "/utf8, Decoder_name/binary>>/binary,
                    "() -> dynamic.Decoder("/utf8>>
            ),
            glam@doc:from_string(<<Type_name/binary, ") "/utf8>>),
            block([Inner_fn])]
    ).

-file("src/squall/internal/codegen.gleam", 133).
?DOC(false).
-spec generate_operation(
    binary(),
    squall@internal@parser:operation(),
    squall@internal@schema:schema(),
    binary()
) -> {ok, binary()} | {error, squall@internal@error:error()}.
generate_operation(Operation_name, Operation, Schema_data, _) ->
    Selections = squall@internal@parser:get_selections(Operation),
    Root_type_name = case squall@internal@parser:get_operation_type(Operation) of
        'query' ->
            erlang:element(2, Schema_data);

        mutation ->
            erlang:element(3, Schema_data);

        subscription ->
            erlang:element(4, Schema_data)
    end,
    gleam@result:'try'(
        begin
            _pipe = Root_type_name,
            gleam@option:to_result(
                _pipe,
                {invalid_schema_response, <<"No root type for operation"/utf8>>}
            )
        end,
        fun(Root_type_name_str) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = gleam@dict:get(
                        erlang:element(5, Schema_data),
                        Root_type_name_str
                    ),
                    gleam@result:map_error(
                        _pipe@1,
                        fun(_) ->
                            {invalid_schema_response,
                                <<"Root type not found: "/utf8,
                                    Root_type_name_str/binary>>}
                        end
                    )
                end,
                fun(Root_type) ->
                    Response_type_name = <<(to_pascal_case(Operation_name))/binary,
                        "Response"/utf8>>,
                    gleam@result:'try'(
                        collect_field_types(Selections, Root_type),
                        fun(Field_types) ->
                            gleam@result:'try'(
                                collect_nested_types(
                                    Selections,
                                    Root_type,
                                    Schema_data
                                ),
                                fun(Nested_types) ->
                                    Nested_docs = begin
                                        _pipe@2 = Nested_types,
                                        _pipe@3 = gleam@list:map(
                                            _pipe@2,
                                            fun(Nested_info) ->
                                                Type_doc = generate_type_definition(
                                                    erlang:element(
                                                        2,
                                                        Nested_info
                                                    ),
                                                    erlang:element(
                                                        3,
                                                        Nested_info
                                                    )
                                                ),
                                                Decoder_doc = generate_decoder_with_schema(
                                                    erlang:element(
                                                        2,
                                                        Nested_info
                                                    ),
                                                    erlang:element(
                                                        3,
                                                        Nested_info
                                                    ),
                                                    erlang:element(
                                                        4,
                                                        Nested_info
                                                    )
                                                ),
                                                [Type_doc, Decoder_doc]
                                            end
                                        ),
                                        gleam@list:flatten(_pipe@3)
                                    end,
                                    Type_def = generate_type_definition(
                                        Response_type_name,
                                        Field_types
                                    ),
                                    Decoder = generate_decoder_with_schema(
                                        Response_type_name,
                                        Field_types,
                                        erlang:element(5, Schema_data)
                                    ),
                                    Variables = squall@internal@parser:get_variables(
                                        Operation
                                    ),
                                    Function_def = generate_function(
                                        Operation_name,
                                        Response_type_name,
                                        Variables,
                                        build_query_string(Operation)
                                    ),
                                    Imports = imports_doc(),
                                    All_docs = begin
                                        _pipe@4 = [Imports | Nested_docs],
                                        lists:append(
                                            _pipe@4,
                                            [Type_def, Decoder, Function_def]
                                        )
                                    end,
                                    Code = begin
                                        _pipe@5 = All_docs,
                                        _pipe@6 = glam@doc:join(
                                            _pipe@5,
                                            glam@doc:lines(2)
                                        ),
                                        _pipe@7 = glam@doc:append(
                                            _pipe@6,
                                            {line, 1}
                                        ),
                                        glam@doc:to_string(_pipe@7, 80)
                                    end,
                                    {ok, Code}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
