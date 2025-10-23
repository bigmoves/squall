-module(squall@internal@parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/squall/internal/parser.gleam").
-export([get_operation_type/1, get_operation_name/1, get_variables/1, get_selections/1, get_variable_name/1, parse/1]).
-export_type([operation_type/0, operation/0, variable/0, type_ref/0, selection/0, argument/0, value/0, token/0, token_position/0, parser_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type operation_type() :: 'query' | mutation | subscription.

-type operation() :: {operation,
        operation_type(),
        gleam@option:option(binary()),
        list(variable()),
        list(selection())}.

-type variable() :: {variable, binary(), type_ref()}.

-type type_ref() :: {named_type_ref, binary()} |
    {list_type_ref, type_ref()} |
    {non_null_type_ref, type_ref()}.

-type selection() :: {field_selection,
        binary(),
        gleam@option:option(binary()),
        list(argument()),
        list(selection())}.

-type argument() :: {argument, binary(), value()}.

-type value() :: {int_value, integer()} |
    {float_value, float()} |
    {string_value, binary()} |
    {boolean_value, boolean()} |
    null_value |
    {variable_value, binary()} |
    {list_value, list(value())} |
    {object_value, list({binary(), value()})}.

-type token() :: left_brace |
    right_brace |
    left_paren |
    right_paren |
    left_bracket |
    right_bracket |
    colon |
    comma |
    exclamation |
    dollar |
    equals |
    at |
    {name, binary()} |
    {string_lit, binary()} |
    {int_lit, integer()} |
    {float_lit, float()} |
    e_o_f.

-type token_position() :: {token_position, token(), integer(), integer()}.

-type parser_state() :: {parser_state, list(token_position()), integer()}.

-file("src/squall/internal/parser.gleam", 94).
?DOC(false).
-spec get_operation_type(operation()) -> operation_type().
get_operation_type(Op) ->
    erlang:element(2, Op).

-file("src/squall/internal/parser.gleam", 98).
?DOC(false).
-spec get_operation_name(operation()) -> gleam@option:option(binary()).
get_operation_name(Op) ->
    erlang:element(3, Op).

-file("src/squall/internal/parser.gleam", 102).
?DOC(false).
-spec get_variables(operation()) -> list(variable()).
get_variables(Op) ->
    erlang:element(4, Op).

-file("src/squall/internal/parser.gleam", 106).
?DOC(false).
-spec get_selections(operation()) -> list(selection()).
get_selections(Op) ->
    erlang:element(5, Op).

-file("src/squall/internal/parser.gleam", 110).
?DOC(false).
-spec get_variable_name(variable()) -> binary().
get_variable_name(Var) ->
    erlang:element(2, Var).

-file("src/squall/internal/parser.gleam", 259).
?DOC(false).
-spec read_string(binary(), binary(), integer()) -> {ok,
        {binary(), binary(), integer()}} |
    {error, squall@internal@error:error()}.
read_string(Source, Acc, Col) ->
    case gleam@string:pop_grapheme(Source) of
        {error, _} ->
            {error,
                {invalid_graph_q_l_syntax,
                    <<"string"/utf8>>,
                    0,
                    <<"Unterminated string"/utf8>>}};

        {ok, {<<"\""/utf8>>, Rest}} ->
            {ok, {Acc, Rest, Col + 1}};

        {ok, {<<"\\"/utf8>>, Rest@1}} ->
            case gleam@string:pop_grapheme(Rest@1) of
                {ok, {Escaped, Rest2}} ->
                    read_string(Rest2, <<Acc/binary, Escaped/binary>>, Col + 2);

                {error, _} ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"string"/utf8>>,
                            0,
                            <<"Unterminated string"/utf8>>}}
            end;

        {ok, {Char, Rest@2}} ->
            read_string(Rest@2, <<Acc/binary, Char/binary>>, Col + 1)
    end.

-file("src/squall/internal/parser.gleam", 366).
?DOC(false).
-spec is_alpha(binary()) -> boolean().
is_alpha(Char) ->
    case Char of
        <<"a"/utf8>> ->
            true;

        <<"b"/utf8>> ->
            true;

        <<"c"/utf8>> ->
            true;

        <<"d"/utf8>> ->
            true;

        <<"e"/utf8>> ->
            true;

        <<"f"/utf8>> ->
            true;

        <<"g"/utf8>> ->
            true;

        <<"h"/utf8>> ->
            true;

        <<"i"/utf8>> ->
            true;

        <<"j"/utf8>> ->
            true;

        <<"k"/utf8>> ->
            true;

        <<"l"/utf8>> ->
            true;

        <<"m"/utf8>> ->
            true;

        <<"n"/utf8>> ->
            true;

        <<"o"/utf8>> ->
            true;

        <<"p"/utf8>> ->
            true;

        <<"q"/utf8>> ->
            true;

        <<"r"/utf8>> ->
            true;

        <<"s"/utf8>> ->
            true;

        <<"t"/utf8>> ->
            true;

        <<"u"/utf8>> ->
            true;

        <<"v"/utf8>> ->
            true;

        <<"w"/utf8>> ->
            true;

        <<"x"/utf8>> ->
            true;

        <<"y"/utf8>> ->
            true;

        <<"z"/utf8>> ->
            true;

        <<"A"/utf8>> ->
            true;

        <<"B"/utf8>> ->
            true;

        <<"C"/utf8>> ->
            true;

        <<"D"/utf8>> ->
            true;

        <<"E"/utf8>> ->
            true;

        <<"F"/utf8>> ->
            true;

        <<"G"/utf8>> ->
            true;

        <<"H"/utf8>> ->
            true;

        <<"I"/utf8>> ->
            true;

        <<"J"/utf8>> ->
            true;

        <<"K"/utf8>> ->
            true;

        <<"L"/utf8>> ->
            true;

        <<"M"/utf8>> ->
            true;

        <<"N"/utf8>> ->
            true;

        <<"O"/utf8>> ->
            true;

        <<"P"/utf8>> ->
            true;

        <<"Q"/utf8>> ->
            true;

        <<"R"/utf8>> ->
            true;

        <<"S"/utf8>> ->
            true;

        <<"T"/utf8>> ->
            true;

        <<"U"/utf8>> ->
            true;

        <<"V"/utf8>> ->
            true;

        <<"W"/utf8>> ->
            true;

        <<"X"/utf8>> ->
            true;

        <<"Y"/utf8>> ->
            true;

        <<"Z"/utf8>> ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/parser.gleam", 374).
?DOC(false).
-spec is_digit(binary()) -> boolean().
is_digit(Char) ->
    case Char of
        <<"0"/utf8>> ->
            true;

        <<"1"/utf8>> ->
            true;

        <<"2"/utf8>> ->
            true;

        <<"3"/utf8>> ->
            true;

        <<"4"/utf8>> ->
            true;

        <<"5"/utf8>> ->
            true;

        <<"6"/utf8>> ->
            true;

        <<"7"/utf8>> ->
            true;

        <<"8"/utf8>> ->
            true;

        <<"9"/utf8>> ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/parser.gleam", 283).
?DOC(false).
-spec read_name_helper(binary(), binary(), integer()) -> {binary(),
    binary(),
    integer()}.
read_name_helper(Source, Acc, Col) ->
    case gleam@string:pop_grapheme(Source) of
        {error, _} ->
            {Acc, <<""/utf8>>, Col};

        {ok, {Char, Rest}} ->
            case (is_alpha(Char) orelse is_digit(Char)) orelse (Char =:= <<"_"/utf8>>) of
                true ->
                    read_name_helper(Rest, <<Acc/binary, Char/binary>>, Col + 1);

                false ->
                    {Acc, <<Char/binary, Rest/binary>>, Col}
            end
    end.

-file("src/squall/internal/parser.gleam", 279).
?DOC(false).
-spec read_name(binary(), integer()) -> {binary(), binary(), integer()}.
read_name(Source, Col) ->
    read_name_helper(Source, <<""/utf8>>, Col).

-file("src/squall/internal/parser.gleam", 354).
?DOC(false).
-spec read_number_helper(binary(), binary(), integer()) -> {binary(),
    binary(),
    integer()}.
read_number_helper(Source, Acc, Col) ->
    case gleam@string:pop_grapheme(Source) of
        {error, _} ->
            {Acc, <<""/utf8>>, Col};

        {ok, {Char, Rest}} ->
            case (((is_digit(Char) orelse (Char =:= <<"."/utf8>>)) orelse (Char
            =:= <<"-"/utf8>>))
            orelse (Char =:= <<"e"/utf8>>))
            orelse (Char =:= <<"E"/utf8>>) of
                true ->
                    read_number_helper(
                        Rest,
                        <<Acc/binary, Char/binary>>,
                        Col + 1
                    );

                false ->
                    {Acc, <<Char/binary, Rest/binary>>, Col}
            end
    end.

-file("src/squall/internal/parser.gleam", 330).
?DOC(false).
-spec try_parse_int(binary()) -> {ok, integer()} | {error, nil}.
try_parse_int(S) ->
    case S of
        <<"0"/utf8>> ->
            {ok, 0};

        <<"1"/utf8>> ->
            {ok, 1};

        <<"2"/utf8>> ->
            {ok, 2};

        <<"3"/utf8>> ->
            {ok, 3};

        <<"4"/utf8>> ->
            {ok, 4};

        <<"5"/utf8>> ->
            {ok, 5};

        <<"6"/utf8>> ->
            {ok, 6};

        <<"7"/utf8>> ->
            {ok, 7};

        <<"8"/utf8>> ->
            {ok, 8};

        <<"9"/utf8>> ->
            {ok, 9};

        _ ->
            erlang:binary_to_integer(S)
    end.

-file("src/squall/internal/parser.gleam", 346).
?DOC(false).
-spec try_parse_float(binary()) -> {ok, float()} | {error, nil}.
try_parse_float(S) ->
    case gleam_stdlib:contains_string(S, <<"."/utf8>>) of
        false ->
            {error, nil};

        true ->
            erlang:binary_to_float(S)
    end.

-file("src/squall/internal/parser.gleam", 295).
?DOC(false).
-spec read_number(binary(), integer(), integer()) -> {ok,
        {token(), binary(), integer()}} |
    {error, squall@internal@error:error()}.
read_number(Source, Col, Line) ->
    {Num_str, Rest, New_col} = read_number_helper(Source, <<""/utf8>>, Col),
    case (gleam_stdlib:contains_string(Num_str, <<"."/utf8>>) orelse gleam_stdlib:contains_string(
        Num_str,
        <<"e"/utf8>>
    ))
    orelse gleam_stdlib:contains_string(Num_str, <<"E"/utf8>>) of
        true ->
            case try_parse_float(Num_str) of
                {ok, F} ->
                    {ok, {{float_lit, F}, Rest, New_col}};

                {error, _} ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"number"/utf8>>,
                            Line,
                            <<"Invalid float: "/utf8, Num_str/binary>>}}
            end;

        false ->
            case try_parse_int(Num_str) of
                {ok, I} ->
                    {ok, {{int_lit, I}, Rest, New_col}};

                {error, _} ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"number"/utf8>>,
                            Line,
                            <<"Invalid int: "/utf8, Num_str/binary>>}}
            end
    end.

-file("src/squall/internal/parser.gleam", 121).
?DOC(false).
-spec tokenize_helper(binary(), integer(), integer(), list(token_position())) -> {ok,
        list(token_position())} |
    {error, squall@internal@error:error()}.
tokenize_helper(Source, Line, Col, Acc) ->
    case gleam@string:pop_grapheme(Source) of
        {error, _} ->
            {ok, [{token_position, e_o_f, Line, Col} | Acc]};

        {ok, {Char, Rest}} ->
            case Char of
                <<" "/utf8>> ->
                    tokenize_helper(Rest, Line, Col + 1, Acc);

                <<"\t"/utf8>> ->
                    tokenize_helper(Rest, Line, Col + 1, Acc);

                <<"\n"/utf8>> ->
                    tokenize_helper(Rest, Line + 1, 1, Acc);

                <<"\r"/utf8>> ->
                    tokenize_helper(Rest, Line, Col, Acc);

                <<"{"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, left_brace, Line, Col} | Acc]
                    );

                <<"}"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, right_brace, Line, Col} | Acc]
                    );

                <<"("/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, left_paren, Line, Col} | Acc]
                    );

                <<")"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, right_paren, Line, Col} | Acc]
                    );

                <<"["/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, left_bracket, Line, Col} | Acc]
                    );

                <<"]"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, right_bracket, Line, Col} | Acc]
                    );

                <<":"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, colon, Line, Col} | Acc]
                    );

                <<","/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, comma, Line, Col} | Acc]
                    );

                <<"!"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, exclamation, Line, Col} | Acc]
                    );

                <<"$"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, dollar, Line, Col} | Acc]
                    );

                <<"="/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, equals, Line, Col} | Acc]
                    );

                <<"@"/utf8>> ->
                    tokenize_helper(
                        Rest,
                        Line,
                        Col + 1,
                        [{token_position, at, Line, Col} | Acc]
                    );

                <<"\""/utf8>> ->
                    gleam@result:'try'(
                        read_string(Rest, <<""/utf8>>, Col + 1),
                        fun(_use0) ->
                            {Str_val, Remaining, New_col} = _use0,
                            tokenize_helper(
                                Remaining,
                                Line,
                                New_col,
                                [{token_position,
                                        {string_lit, Str_val},
                                        Line,
                                        Col} |
                                    Acc]
                            )
                        end
                    );

                _ ->
                    case is_alpha(Char) orelse (Char =:= <<"_"/utf8>>) of
                        true ->
                            {Name, Remaining@1, New_col@1} = read_name(
                                <<Char/binary, Rest/binary>>,
                                Col
                            ),
                            tokenize_helper(
                                Remaining@1,
                                Line,
                                New_col@1,
                                [{token_position, {name, Name}, Line, Col} |
                                    Acc]
                            );

                        false ->
                            case is_digit(Char) orelse (Char =:= <<"-"/utf8>>) of
                                true ->
                                    gleam@result:'try'(
                                        read_number(
                                            <<Char/binary, Rest/binary>>,
                                            Col,
                                            Line
                                        ),
                                        fun(_use0@1) ->
                                            {Num_token, Remaining@2, New_col@2} = _use0@1,
                                            tokenize_helper(
                                                Remaining@2,
                                                Line,
                                                New_col@2,
                                                [{token_position,
                                                        Num_token,
                                                        Line,
                                                        Col} |
                                                    Acc]
                                            )
                                        end
                                    );

                                false ->
                                    {error,
                                        {invalid_graph_q_l_syntax,
                                            <<"tokenize"/utf8>>,
                                            Line,
                                            <<"Unexpected character: "/utf8,
                                                Char/binary>>}}
                            end
                    end
            end
    end.

-file("src/squall/internal/parser.gleam", 116).
?DOC(false).
-spec tokenize(binary()) -> {ok, list(token_position())} |
    {error, squall@internal@error:error()}.
tokenize(Source) ->
    _pipe = tokenize_helper(Source, 1, 1, []),
    gleam@result:map(_pipe, fun lists:reverse/1).

-file("src/squall/internal/parser.gleam", 711).
?DOC(false).
-spec peek_token(parser_state()) -> {ok, token_position()} |
    {error, squall@internal@error:error()}.
peek_token(State) ->
    case gleam@list:drop(erlang:element(2, State), erlang:element(3, State)) of
        [Token | _] ->
            {ok, Token};

        [] ->
            {error,
                {invalid_graph_q_l_syntax,
                    <<"parser"/utf8>>,
                    0,
                    <<"Unexpected end of input"/utf8>>}}
    end.

-file("src/squall/internal/parser.gleam", 718).
?DOC(false).
-spec advance(parser_state()) -> parser_state().
advance(State) ->
    {parser_state, erlang:element(2, State), erlang:element(3, State) + 1}.

-file("src/squall/internal/parser.gleam", 409).
?DOC(false).
-spec parse_operation_type(parser_state()) -> {ok,
        {operation_type(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_operation_type(State) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                {name, <<"query"/utf8>>} ->
                    {ok, {'query', advance(State)}};

                {name, <<"mutation"/utf8>>} ->
                    {ok, {mutation, advance(State)}};

                {name, <<"subscription"/utf8>>} ->
                    {ok, {subscription, advance(State)}};

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"operation"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected 'query', 'mutation', or 'subscription'"/utf8>>}}
            end end
    ).

-file("src/squall/internal/parser.gleam", 427).
?DOC(false).
-spec parse_optional_name(parser_state()) -> {gleam@option:option(binary()),
    parser_state()}.
parse_optional_name(State) ->
    case peek_token(State) of
        {ok, {token_position, {name, Name}, _, _}} ->
            {{some, Name}, advance(State)};

        _ ->
            {none, State}
    end.

-file("src/squall/internal/parser.gleam", 695).
?DOC(false).
-spec parse_name(parser_state()) -> {ok, {binary(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_name(State) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                {name, Name} ->
                    {ok, {Name, advance(State)}};

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"name"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected a name"/utf8>>}}
            end end
    ).

-file("src/squall/internal/parser.gleam", 740).
?DOC(false).
-spec tokens_equal(token(), token()) -> boolean().
tokens_equal(A, B) ->
    case {A, B} of
        {left_brace, left_brace} ->
            true;

        {right_brace, right_brace} ->
            true;

        {left_paren, left_paren} ->
            true;

        {right_paren, right_paren} ->
            true;

        {left_bracket, left_bracket} ->
            true;

        {right_bracket, right_bracket} ->
            true;

        {colon, colon} ->
            true;

        {comma, comma} ->
            true;

        {exclamation, exclamation} ->
            true;

        {dollar, dollar} ->
            true;

        {equals, equals} ->
            true;

        {at, at} ->
            true;

        {e_o_f, e_o_f} ->
            true;

        {_, _} ->
            false
    end.

-file("src/squall/internal/parser.gleam", 759).
?DOC(false).
-spec token_to_string(token()) -> binary().
token_to_string(Token) ->
    case Token of
        left_brace ->
            <<"'{'"/utf8>>;

        right_brace ->
            <<"'}'"/utf8>>;

        left_paren ->
            <<"'('"/utf8>>;

        right_paren ->
            <<"')'"/utf8>>;

        left_bracket ->
            <<"'['"/utf8>>;

        right_bracket ->
            <<"']'"/utf8>>;

        colon ->
            <<"':'"/utf8>>;

        comma ->
            <<"','"/utf8>>;

        exclamation ->
            <<"'!'"/utf8>>;

        dollar ->
            <<"'$'"/utf8>>;

        equals ->
            <<"'='"/utf8>>;

        at ->
            <<"'@'"/utf8>>;

        {name, N} ->
            <<<<"name '"/utf8, N/binary>>/binary, "'"/utf8>>;

        {string_lit, S} ->
            <<<<"string \""/utf8, S/binary>>/binary, "\""/utf8>>;

        {int_lit, _} ->
            <<"integer"/utf8>>;

        {float_lit, _} ->
            <<"float"/utf8>>;

        e_o_f ->
            <<"end of input"/utf8>>
    end.

-file("src/squall/internal/parser.gleam", 722).
?DOC(false).
-spec expect_token(parser_state(), token(), binary()) -> {ok, parser_state()} |
    {error, squall@internal@error:error()}.
expect_token(State, Expected, Context) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) ->
            case tokens_equal(erlang:element(2, Token_pos), Expected) of
                true ->
                    {ok, advance(State)};

                false ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            Context,
                            erlang:element(3, Token_pos),
                            <<"Expected "/utf8,
                                (token_to_string(Expected))/binary>>}}
            end
        end
    ).

-file("src/squall/internal/parser.gleam", 484).
?DOC(false).
-spec parse_type_ref(parser_state()) -> {ok, {type_ref(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_type_ref(State) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                left_bracket ->
                    State@1 = advance(State),
                    gleam@result:'try'(
                        parse_type_ref(State@1),
                        fun(_use0) ->
                            {Inner, State@2} = _use0,
                            gleam@result:'try'(
                                expect_token(
                                    State@2,
                                    right_bracket,
                                    <<"list type"/utf8>>
                                ),
                                fun(State@3) -> case peek_token(State@3) of
                                        {ok,
                                            {token_position, exclamation, _, _}} ->
                                            {ok,
                                                {{non_null_type_ref,
                                                        {list_type_ref, Inner}},
                                                    advance(State@3)}};

                                        _ ->
                                            {ok,
                                                {{list_type_ref, Inner},
                                                    State@3}}
                                    end end
                            )
                        end
                    );

                {name, Name} ->
                    State@4 = advance(State),
                    case peek_token(State@4) of
                        {ok, {token_position, exclamation, _, _}} ->
                            {ok,
                                {{non_null_type_ref, {named_type_ref, Name}},
                                    advance(State@4)}};

                        _ ->
                            {ok, {{named_type_ref, Name}, State@4}}
                    end;

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"type"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected type name or '['"/utf8>>}}
            end end
    ).

-file("src/squall/internal/parser.gleam", 468).
?DOC(false).
-spec parse_variable(parser_state()) -> {ok, {variable(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_variable(State) ->
    gleam@result:'try'(
        expect_token(State, dollar, <<"variable"/utf8>>),
        fun(State@1) ->
            gleam@result:'try'(
                parse_name(State@1),
                fun(_use0) ->
                    {Name, State@2} = _use0,
                    gleam@result:'try'(
                        expect_token(State@2, colon, <<"variable type"/utf8>>),
                        fun(State@3) ->
                            gleam@result:'try'(
                                parse_type_ref(State@3),
                                fun(_use0@1) ->
                                    {Type_ref, State@4} = _use0@1,
                                    {ok, {{variable, Name, Type_ref}, State@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/squall/internal/parser.gleam", 446).
?DOC(false).
-spec parse_variable_list(parser_state(), list(variable())) -> {ok,
        {list(variable()), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_variable_list(State, Acc) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                right_paren ->
                    {ok, {lists:reverse(Acc), advance(State)}};

                comma ->
                    parse_variable_list(advance(State), Acc);

                dollar ->
                    gleam@result:'try'(
                        parse_variable(State),
                        fun(_use0) ->
                            {Var, State@1} = _use0,
                            parse_variable_list(State@1, [Var | Acc])
                        end
                    );

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"variables"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected '$' or ')'"/utf8>>}}
            end end
    ).

-file("src/squall/internal/parser.gleam", 434).
?DOC(false).
-spec parse_variable_definitions(parser_state()) -> {ok,
        {list(variable()), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_variable_definitions(State) ->
    case peek_token(State) of
        {ok, {token_position, left_paren, _, _}} ->
            State@1 = advance(State),
            parse_variable_list(State@1, []);

        _ ->
            {ok, {[], State}}
    end.

-file("src/squall/internal/parser.gleam", 781).
?DOC(false).
-spec skip_insignificant(parser_state()) -> parser_state().
skip_insignificant(State) ->
    State.

-file("src/squall/internal/parser.gleam", 549).
?DOC(false).
-spec parse_field(parser_state()) -> {ok, {selection(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_field(State) ->
    gleam@result:'try'(
        parse_name(State),
        fun(_use0) ->
            {Name, State@1} = _use0,
            {Alias, Field_name, State@4} = case peek_token(State@1) of
                {ok, {token_position, colon, _, _}} ->
                    State@2 = advance(State@1),
                    case parse_name(State@2) of
                        {ok, {Real_name, State@3}} ->
                            {{some, Name}, Real_name, State@3};

                        {error, _} ->
                            {none, Name, State@2}
                    end;

                _ ->
                    {none, Name, State@1}
            end,
            gleam@result:'try'(
                parse_arguments(State@4),
                fun(_use0@1) ->
                    {Arguments, State@5} = _use0@1,
                    {Selections, State@7} = case peek_token(State@5) of
                        {ok, {token_position, left_brace, _, _}} ->
                            case parse_selection_set(State@5) of
                                {ok, {Sels, State@6}} ->
                                    {Sels, State@6};

                                {error, _} ->
                                    {[], State@5}
                            end;

                        _ ->
                            {[], State@5}
                    end,
                    {ok,
                        {{field_selection,
                                Field_name,
                                Alias,
                                Arguments,
                                Selections},
                            State@7}}
                end
            )
        end
    ).

-file("src/squall/internal/parser.gleam", 521).
?DOC(false).
-spec parse_selection_set(parser_state()) -> {ok,
        {list(selection()), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_selection_set(State) ->
    gleam@result:'try'(
        expect_token(State, left_brace, <<"selection set"/utf8>>),
        fun(State@1) -> parse_selection_list(State@1, []) end
    ).

-file("src/squall/internal/parser.gleam", 528).
?DOC(false).
-spec parse_selection_list(parser_state(), list(selection())) -> {ok,
        {list(selection()), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_selection_list(State, Acc) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                right_brace ->
                    {ok, {lists:reverse(Acc), advance(State)}};

                {name, _} ->
                    gleam@result:'try'(
                        parse_field(State),
                        fun(_use0) ->
                            {Selection, State@1} = _use0,
                            parse_selection_list(State@1, [Selection | Acc])
                        end
                    );

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"selection"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected field name or '}'"/utf8>>}}
            end end
    ).

-file("src/squall/internal/parser.gleam", 390).
?DOC(false).
-spec parse_operation(parser_state()) -> {ok, operation()} |
    {error, squall@internal@error:error()}.
parse_operation(State) ->
    State@1 = skip_insignificant(State),
    gleam@result:'try'(
        parse_operation_type(State@1),
        fun(_use0) ->
            {Op_type, State@2} = _use0,
            {Op_name, State@3} = parse_optional_name(State@2),
            gleam@result:'try'(
                parse_variable_definitions(State@3),
                fun(_use0@1) ->
                    {Variables, State@4} = _use0@1,
                    gleam@result:'try'(
                        parse_selection_set(State@4),
                        fun(_use0@2) ->
                            {Selections, _} = _use0@2,
                            {ok,
                                {operation,
                                    Op_type,
                                    Op_name,
                                    Variables,
                                    Selections}}
                        end
                    )
                end
            )
        end
    ).

-file("src/squall/internal/parser.gleam", 89).
?DOC(false).
-spec parse(binary()) -> {ok, operation()} |
    {error, squall@internal@error:error()}.
parse(Source) ->
    gleam@result:'try'(
        tokenize(Source),
        fun(Tokens) -> parse_operation({parser_state, Tokens, 0}) end
    ).

-file("src/squall/internal/parser.gleam", 655).
?DOC(false).
-spec parse_list_value(parser_state(), list(value())) -> {ok,
        {value(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_list_value(State, Acc) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                right_bracket ->
                    {ok, {{list_value, lists:reverse(Acc)}, advance(State)}};

                comma ->
                    parse_list_value(advance(State), Acc);

                _ ->
                    gleam@result:'try'(
                        parse_value(State),
                        fun(_use0) ->
                            {Value, State@1} = _use0,
                            parse_list_value(State@1, [Value | Acc])
                        end
                    )
            end end
    ).

-file("src/squall/internal/parser.gleam", 623).
?DOC(false).
-spec parse_value(parser_state()) -> {ok, {value(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_value(State) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                {int_lit, I} ->
                    {ok, {{int_value, I}, advance(State)}};

                {float_lit, F} ->
                    {ok, {{float_value, F}, advance(State)}};

                {string_lit, S} ->
                    {ok, {{string_value, S}, advance(State)}};

                {name, <<"true"/utf8>>} ->
                    {ok, {{boolean_value, true}, advance(State)}};

                {name, <<"false"/utf8>>} ->
                    {ok, {{boolean_value, false}, advance(State)}};

                {name, <<"null"/utf8>>} ->
                    {ok, {null_value, advance(State)}};

                dollar ->
                    State@1 = advance(State),
                    gleam@result:'try'(
                        parse_name(State@1),
                        fun(_use0) ->
                            {Name, State@2} = _use0,
                            {ok, {{variable_value, Name}, State@2}}
                        end
                    );

                left_bracket ->
                    State@3 = advance(State),
                    parse_list_value(State@3, []);

                left_brace ->
                    State@4 = advance(State),
                    parse_object_value(State@4, []);

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"value"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected a value"/utf8>>}}
            end end
    ).

-file("src/squall/internal/parser.gleam", 615).
?DOC(false).
-spec parse_argument(parser_state()) -> {ok, {argument(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_argument(State) ->
    gleam@result:'try'(
        parse_name(State),
        fun(_use0) ->
            {Name, State@1} = _use0,
            gleam@result:'try'(
                expect_token(State@1, colon, <<"argument value"/utf8>>),
                fun(State@2) ->
                    gleam@result:'try'(
                        parse_value(State@2),
                        fun(_use0@1) ->
                            {Value, State@3} = _use0@1,
                            {ok, {{argument, Name, Value}, State@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/squall/internal/parser.gleam", 593).
?DOC(false).
-spec parse_argument_list(parser_state(), list(argument())) -> {ok,
        {list(argument()), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_argument_list(State, Acc) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                right_paren ->
                    {ok, {lists:reverse(Acc), advance(State)}};

                comma ->
                    parse_argument_list(advance(State), Acc);

                {name, _} ->
                    gleam@result:'try'(
                        parse_argument(State),
                        fun(_use0) ->
                            {Arg, State@1} = _use0,
                            parse_argument_list(State@1, [Arg | Acc])
                        end
                    );

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"argument"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected argument name or ')'"/utf8>>}}
            end end
    ).

-file("src/squall/internal/parser.gleam", 581).
?DOC(false).
-spec parse_arguments(parser_state()) -> {ok,
        {list(argument()), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_arguments(State) ->
    case peek_token(State) of
        {ok, {token_position, left_paren, _, _}} ->
            State@1 = advance(State),
            parse_argument_list(State@1, []);

        _ ->
            {ok, {[], State}}
    end.

-file("src/squall/internal/parser.gleam", 671).
?DOC(false).
-spec parse_object_value(parser_state(), list({binary(), value()})) -> {ok,
        {value(), parser_state()}} |
    {error, squall@internal@error:error()}.
parse_object_value(State, Acc) ->
    gleam@result:'try'(
        peek_token(State),
        fun(Token_pos) -> case erlang:element(2, Token_pos) of
                right_brace ->
                    {ok, {{object_value, lists:reverse(Acc)}, advance(State)}};

                comma ->
                    parse_object_value(advance(State), Acc);

                {name, _} ->
                    gleam@result:'try'(
                        parse_name(State),
                        fun(_use0) ->
                            {Name, State@1} = _use0,
                            gleam@result:'try'(
                                expect_token(
                                    State@1,
                                    colon,
                                    <<"object field value"/utf8>>
                                ),
                                fun(State@2) ->
                                    gleam@result:'try'(
                                        parse_value(State@2),
                                        fun(_use0@1) ->
                                            {Value, State@3} = _use0@1,
                                            parse_object_value(
                                                State@3,
                                                [{Name, Value} | Acc]
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    );

                _ ->
                    {error,
                        {invalid_graph_q_l_syntax,
                            <<"object"/utf8>>,
                            erlang:element(3, Token_pos),
                            <<"Expected field name or '}'"/utf8>>}}
            end end
    ).
