-module(squall@internal@error).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/squall/internal/error.gleam").
-export([to_string/1]).
-export_type([error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type error() :: {http_request_failed, binary()} |
    {http_invalid_response, integer(), binary()} |
    {schema_introspection_failed, binary()} |
    {invalid_schema_response, binary()} |
    {cannot_read_file, binary(), binary()} |
    {cannot_write_to_file, binary(), binary()} |
    {invalid_graph_q_l_syntax, binary(), integer(), binary()} |
    {invalid_operation_name, binary(), binary(), binary()} |
    {unsupported_graph_q_l_type, binary(), binary()} |
    {invalid_type_mapping, binary()} |
    {code_generation_failed, binary()}.

-file("src/squall/internal/error.gleam", 60).
?DOC(false).
-spec int_to_string(integer()) -> binary().
int_to_string(I) ->
    case I of
        _ ->
            S@1 = case begin
                _pipe = gleam@json:to_string(gleam@json:int(I)),
                {ok, _pipe}
            end of
                {ok, S} -> S;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"squall/internal/error"/utf8>>,
                                function => <<"int_to_string"/utf8>>,
                                line => 64,
                                value => _assert_fail,
                                start => 2023,
                                'end' => 2075,
                                pattern_start => 2034,
                                pattern_end => 2039})
            end,
            S@1
    end.

-file("src/squall/internal/error.gleam", 28).
?DOC(false).
-spec to_string(error()) -> binary().
to_string(Error) ->
    case Error of
        {http_request_failed, Reason} ->
            <<"HTTP request failed: "/utf8, Reason/binary>>;

        {http_invalid_response, Status, Body} ->
            <<<<<<"Invalid HTTP response (status "/utf8,
                        (int_to_string(Status))/binary>>/binary,
                    "): "/utf8>>/binary,
                Body/binary>>;

        {schema_introspection_failed, Reason@1} ->
            <<"Schema introspection failed: "/utf8, Reason@1/binary>>;

        {invalid_schema_response, Reason@2} ->
            <<"Invalid schema response: "/utf8, Reason@2/binary>>;

        {cannot_read_file, File, Reason@3} ->
            <<<<<<"Cannot read file "/utf8, File/binary>>/binary, ": "/utf8>>/binary,
                Reason@3/binary>>;

        {cannot_write_to_file, File@1, Reason@4} ->
            <<<<<<"Cannot write to file "/utf8, File@1/binary>>/binary,
                    ": "/utf8>>/binary,
                Reason@4/binary>>;

        {invalid_graph_q_l_syntax, File@2, Line, Message} ->
            <<<<<<<<<<"Invalid GraphQL syntax in "/utf8, File@2/binary>>/binary,
                            " at line "/utf8>>/binary,
                        (int_to_string(Line))/binary>>/binary,
                    ": "/utf8>>/binary,
                Message/binary>>;

        {invalid_operation_name, File@3, Name, Reason@5} ->
            <<<<<<<<<<"Invalid operation name '"/utf8, Name/binary>>/binary,
                            "' in "/utf8>>/binary,
                        File@3/binary>>/binary,
                    ": "/utf8>>/binary,
                Reason@5/binary>>;

        {unsupported_graph_q_l_type, File@4, Type_name} ->
            <<<<<<"Unsupported GraphQL type '"/utf8, Type_name/binary>>/binary,
                    "' in "/utf8>>/binary,
                File@4/binary>>;

        {invalid_type_mapping, Reason@6} ->
            <<"Invalid type mapping: "/utf8, Reason@6/binary>>;

        {code_generation_failed, Reason@7} ->
            <<"Code generation failed: "/utf8, Reason@7/binary>>
    end.
