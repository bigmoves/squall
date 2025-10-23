-module(squall@internal@discovery).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/squall/internal/discovery.gleam").
-export([extract_operation_name/1, find_graphql_files/1]).
-export_type([graph_q_l_file/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type graph_q_l_file() :: {graph_q_l_file, binary(), binary(), binary()}.

-file("src/squall/internal/discovery.gleam", 89).
?DOC(false).
-spec is_lowercase_letter(binary()) -> boolean().
is_lowercase_letter(Char) ->
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

        _ ->
            false
    end.

-file("src/squall/internal/discovery.gleam", 97).
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

-file("src/squall/internal/discovery.gleam", 70).
?DOC(false).
-spec is_valid_gleam_identifier(binary()) -> boolean().
is_valid_gleam_identifier(Name) ->
    case gleam@string:to_graphemes(Name) of
        [] ->
            false;

        [First | Rest] ->
            First_valid = is_lowercase_letter(First),
            Rest_valid = begin
                _pipe = Rest,
                gleam@list:all(
                    _pipe,
                    fun(Char) ->
                        (is_lowercase_letter(Char) orelse is_digit(Char)) orelse (Char
                        =:= <<"_"/utf8>>)
                    end
                )
            end,
            First_valid andalso Rest_valid
    end.

-file("src/squall/internal/discovery.gleam", 47).
?DOC(false).
-spec extract_operation_name(binary()) -> {ok, binary()} |
    {error, squall@internal@error:error()}.
extract_operation_name(Path) ->
    Filename = filepath:base_name(Path),
    Name = case gleam@string:ends_with(Filename, <<".gql"/utf8>>) of
        true ->
            gleam@string:drop_right(Filename, 4);

        false ->
            Filename
    end,
    case is_valid_gleam_identifier(Name) of
        true ->
            {ok, Name};

        false ->
            {error,
                {invalid_operation_name,
                    Path,
                    Name,
                    <<"Must start with lowercase letter and contain only letters, numbers, and underscores"/utf8>>}}
    end.

-file("src/squall/internal/discovery.gleam", 105).
?DOC(false).
-spec walk_directory(binary()) -> {ok, list(binary())} |
    {error, simplifile:file_error()}.
walk_directory(Path) ->
    gleam@result:'try'(
        simplifile_erl:is_directory(Path),
        fun(Is_dir) -> case Is_dir of
                false ->
                    {ok, [Path]};

                true ->
                    gleam@result:'try'(
                        simplifile_erl:read_directory(Path),
                        fun(Entries) -> _pipe = Entries,
                            _pipe@1 = gleam@list:try_map(
                                _pipe,
                                fun(Entry) ->
                                    Full_path = filepath:join(Path, Entry),
                                    walk_directory(Full_path)
                                end
                            ),
                            gleam@result:map(_pipe@1, fun gleam@list:flatten/1) end
                    )
            end end
    ).

-file("src/squall/internal/discovery.gleam", 123).
?DOC(false).
-spec simplifile_error(simplifile:file_error()) -> binary().
simplifile_error(Err) ->
    case Err of
        enoent ->
            <<"File or directory not found"/utf8>>;

        eacces ->
            <<"Permission denied"/utf8>>;

        epipe ->
            <<"Broken pipe"/utf8>>;

        eexist ->
            <<"File already exists"/utf8>>;

        enotdir ->
            <<"Not a directory"/utf8>>;

        eisdir ->
            <<"Is a directory"/utf8>>;

        {unknown, Msg} ->
            <<"Unknown error: "/utf8, Msg/binary>>;

        _ ->
            <<"File system error"/utf8>>
    end.

-file("src/squall/internal/discovery.gleam", 34).
?DOC(false).
-spec read_graphql_file(binary()) -> {ok, graph_q_l_file()} |
    {error, squall@internal@error:error()}.
read_graphql_file(Path) ->
    gleam@result:'try'(
        extract_operation_name(Path),
        fun(Operation_name) ->
            gleam@result:'try'(
                begin
                    _pipe = simplifile:read(Path),
                    gleam@result:map_error(
                        _pipe,
                        fun(Err) ->
                            {cannot_read_file, Path, simplifile_error(Err)}
                        end
                    )
                end,
                fun(Content) ->
                    {ok, {graph_q_l_file, Path, Operation_name, Content}}
                end
            )
        end
    ).

-file("src/squall/internal/discovery.gleam", 13).
?DOC(false).
-spec find_graphql_files(binary()) -> {ok, list(graph_q_l_file())} |
    {error, squall@internal@error:error()}.
find_graphql_files(Root) ->
    case walk_directory(Root) of
        {ok, Paths} ->
            Gql_files = begin
                _pipe = Paths,
                _pipe@1 = gleam@list:filter(
                    _pipe,
                    fun(Path) ->
                        gleam@string:ends_with(Path, <<".gql"/utf8>>) andalso gleam_stdlib:contains_string(
                            Path,
                            <<"/graphql/"/utf8>>
                        )
                    end
                ),
                gleam@list:sort(_pipe@1, fun gleam@string:compare/2)
            end,
            _pipe@2 = Gql_files,
            gleam@list:try_map(_pipe@2, fun read_graphql_file/1);

        {error, Err} ->
            {error, {cannot_read_file, Root, simplifile_error(Err)}}
    end.
