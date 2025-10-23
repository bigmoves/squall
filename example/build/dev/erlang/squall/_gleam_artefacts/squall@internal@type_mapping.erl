-module(squall@internal@type_mapping).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/squall/internal/type_mapping.gleam").
-export([graphql_to_gleam/1, graphql_to_gleam_nullable/1, parser_type_to_schema_type/1, is_string_type/1, is_int_type/1, is_float_type/1, is_bool_type/1, is_list_type/1, is_option_type/1, is_custom_type/1, get_type_name/1, to_gleam_type_string/1]).
-export_type([gleam_type/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type gleam_type() :: string_type |
    int_type |
    float_type |
    bool_type |
    {list_type, gleam_type()} |
    {option_type, gleam_type()} |
    {custom_type, binary()}.

-file("src/squall/internal/type_mapping.gleam", 57).
?DOC(false).
-spec map_scalar_type(binary()) -> {ok, gleam_type()} |
    {error, squall@internal@error:error()}.
map_scalar_type(Name) ->
    case Name of
        <<"String"/utf8>> ->
            {ok, string_type};

        <<"Int"/utf8>> ->
            {ok, int_type};

        <<"Float"/utf8>> ->
            {ok, float_type};

        <<"Boolean"/utf8>> ->
            {ok, bool_type};

        <<"ID"/utf8>> ->
            {ok, string_type};

        _ ->
            {ok, string_type}
    end.

-file("src/squall/internal/type_mapping.gleam", 44).
?DOC(false).
-spec map_named_type(binary(), squall@internal@schema:type_kind()) -> {ok,
        gleam_type()} |
    {error, squall@internal@error:error()}.
map_named_type(Name, Kind) ->
    case Kind of
        scalar ->
            map_scalar_type(Name);

        object ->
            {ok, {custom_type, Name}};

        interface ->
            {ok, {custom_type, Name}};

        union ->
            {ok, {custom_type, Name}};

        enum ->
            {ok, {custom_type, Name}};

        input_object ->
            {ok, {custom_type, Name}};

        _ ->
            {error, {unsupported_graph_q_l_type, <<"type_mapping"/utf8>>, Name}}
    end.

-file("src/squall/internal/type_mapping.gleam", 19).
?DOC(false).
-spec graphql_to_gleam(squall@internal@schema:type_ref()) -> {ok, gleam_type()} |
    {error, squall@internal@error:error()}.
graphql_to_gleam(Type_ref) ->
    case Type_ref of
        {named_type, Name, Kind} ->
            map_named_type(Name, Kind);

        {list_type, Inner} ->
            gleam@result:'try'(
                graphql_to_gleam(Inner),
                fun(Inner_gleam) -> {ok, {list_type, Inner_gleam}} end
            );

        {non_null_type, Inner@1} ->
            graphql_to_gleam(Inner@1)
    end.

-file("src/squall/internal/type_mapping.gleam", 31).
?DOC(false).
-spec graphql_to_gleam_nullable(squall@internal@schema:type_ref()) -> {ok,
        gleam_type()} |
    {error, squall@internal@error:error()}.
graphql_to_gleam_nullable(Type_ref) ->
    case Type_ref of
        {non_null_type, Inner} ->
            graphql_to_gleam(Inner);

        _ ->
            gleam@result:'try'(
                graphql_to_gleam(Type_ref),
                fun(Gleam_type) -> {ok, {option_type, Gleam_type}} end
            )
    end.

-file("src/squall/internal/type_mapping.gleam", 70).
?DOC(false).
-spec parser_type_to_schema_type(squall@internal@parser:type_ref()) -> {ok,
        squall@internal@schema:type_ref()} |
    {error, squall@internal@error:error()}.
parser_type_to_schema_type(Parser_type) ->
    case Parser_type of
        {named_type_ref, Name} ->
            {ok, {named_type, Name, scalar}};

        {list_type_ref, Inner} ->
            gleam@result:'try'(
                parser_type_to_schema_type(Inner),
                fun(Inner_schema) -> {ok, {list_type, Inner_schema}} end
            );

        {non_null_type_ref, Inner@1} ->
            gleam@result:'try'(
                parser_type_to_schema_type(Inner@1),
                fun(Inner_schema@1) -> {ok, {non_null_type, Inner_schema@1}} end
            )
    end.

-file("src/squall/internal/type_mapping.gleam", 88).
?DOC(false).
-spec is_string_type(gleam_type()) -> boolean().
is_string_type(Gleam_type) ->
    case Gleam_type of
        string_type ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/type_mapping.gleam", 95).
?DOC(false).
-spec is_int_type(gleam_type()) -> boolean().
is_int_type(Gleam_type) ->
    case Gleam_type of
        int_type ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/type_mapping.gleam", 102).
?DOC(false).
-spec is_float_type(gleam_type()) -> boolean().
is_float_type(Gleam_type) ->
    case Gleam_type of
        float_type ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/type_mapping.gleam", 109).
?DOC(false).
-spec is_bool_type(gleam_type()) -> boolean().
is_bool_type(Gleam_type) ->
    case Gleam_type of
        bool_type ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/type_mapping.gleam", 116).
?DOC(false).
-spec is_list_type(gleam_type()) -> boolean().
is_list_type(Gleam_type) ->
    case Gleam_type of
        {list_type, _} ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/type_mapping.gleam", 123).
?DOC(false).
-spec is_option_type(gleam_type()) -> boolean().
is_option_type(Gleam_type) ->
    case Gleam_type of
        {option_type, _} ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/type_mapping.gleam", 130).
?DOC(false).
-spec is_custom_type(gleam_type()) -> boolean().
is_custom_type(Gleam_type) ->
    case Gleam_type of
        {custom_type, _} ->
            true;

        _ ->
            false
    end.

-file("src/squall/internal/type_mapping.gleam", 137).
?DOC(false).
-spec get_type_name(gleam_type()) -> gleam@option:option(binary()).
get_type_name(Gleam_type) ->
    case Gleam_type of
        {custom_type, Name} ->
            {some, Name};

        _ ->
            none
    end.

-file("src/squall/internal/type_mapping.gleam", 145).
?DOC(false).
-spec to_gleam_type_string(gleam_type()) -> binary().
to_gleam_type_string(Gleam_type) ->
    case Gleam_type of
        string_type ->
            <<"String"/utf8>>;

        int_type ->
            <<"Int"/utf8>>;

        float_type ->
            <<"Float"/utf8>>;

        bool_type ->
            <<"Bool"/utf8>>;

        {list_type, Inner} ->
            <<<<"List("/utf8, (to_gleam_type_string(Inner))/binary>>/binary,
                ")"/utf8>>;

        {option_type, Inner@1} ->
            <<<<"Option("/utf8, (to_gleam_type_string(Inner@1))/binary>>/binary,
                ")"/utf8>>;

        {custom_type, Name} ->
            Name
    end.
