-module(glam@doc).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/glam/doc.gleam").
-export([append/2, break/2, concat/1, append_docs/2, flex_break/2, force_break/1, from_string/1, zero_width_string/1, group/1, join/2, concat_join/2, lines/1, nest/2, nest_docs/2, prepend/2, prepend_docs/2, to_string/2, debug/1]).
-export_type([document/0, mode/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque document() :: {line, integer()} |
    {concat, list(document())} |
    {text, binary(), integer()} |
    {nest, document(), integer()} |
    {force_break, document()} |
    {break, binary(), binary()} |
    {flex_break, binary(), binary()} |
    {group, document()}.

-type mode() :: broken | force_broken | unbroken.

-file("src/glam/doc.gleam", 30).
?DOC(
    " Joins a document into the end of another.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_string(\"pretty\")\n"
    " |> append(from_string(\" printer\"))\n"
    " |> to_string(80)\n"
    " // -> \"pretty printer\"\n"
    " ```\n"
).
-spec append(document(), document()) -> document().
append(First, Second) ->
    case First of
        {concat, Docs} ->
            {concat, lists:append(Docs, [Second])};

        _ ->
            {concat, [First, Second]}
    end.

-file("src/glam/doc.gleam", 81).
?DOC(
    " A document after which the pretty printer can insert a new line.\n"
    " A newline is added after a `break` document if the `group` it's part of\n"
    " could not be rendered on a single line.\n"
    "\n"
    " If the pretty printer decides to add a newline after `break` it will be\n"
    " rendered as its second argument, otherwise as its first argument.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let message =\n"
    "   [from_string(\"pretty\"), break(\"•\", \"↩\"), from_string(\"printed\")]\n"
    "   |> concat\n"
    "   |> group\n"
    "\n"
    " message |> to_string(20)\n"
    " // -> \"pretty•printed\"\n"
    "\n"
    " message |> to_string(10)\n"
    " // -> \"pretty↩\n"
    " // printed\"\n"
    " ```\n"
).
-spec break(binary(), binary()) -> document().
break(Unbroken, Broken) ->
    {break, Unbroken, Broken}.

-file("src/glam/doc.gleam", 105).
?DOC(
    " Joins a list of documents into a single document.\n"
    "\n"
    " The resulting pretty printed document would be the same as pretty printing\n"
    " each document separately and concatenating it together with `<>`:\n"
    "\n"
    " ```gleam\n"
    " docs |> concat |> to_string(n) ==\n"
    " docs |> list.map(to_string(n)) |> string.concat\n"
    " ```\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\"pretty\", \" \", \"printed\"]\n"
    " |> list.map(from_string)\n"
    " |> concat\n"
    " |> to_string(80)\n"
    " // -> \"pretty printed\"\n"
    " ```\n"
).
-spec concat(list(document())) -> document().
concat(Docs) ->
    {concat, Docs}.

-file("src/glam/doc.gleam", 54).
?DOC(
    " Joins multiple documents into the end of another.\n"
    "\n"
    " This is a shorthand for `append(to: first, doc: concat(docs))`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_string(\"pretty\")\n"
    " |> append_docs([\n"
    "   from_string(\"printing\"),\n"
    "   space,\n"
    "   from_string(\"rocks!\"),\n"
    " ])\n"
    " |> to_string(80)\n"
    " // -> \"pretty printing rocks!\"\n"
    " ```\n"
).
-spec append_docs(document(), list(document())) -> document().
append_docs(First, Docs) ->
    append(First, concat(Docs)).

-file("src/glam/doc.gleam", 169).
?DOC(
    " A document after which the pretty printer can insert a new line.\n"
    " The difference with a simple `break` is that, the pretty printer will decide\n"
    " wether to add a new line or not on a space-by-space basis.\n"
    "\n"
    " While all the `break` inside a group are broken or not, some `flex_breaks`\n"
    " may be broken and some not, depending wether the document can fit on a\n"
    " single line or not. Hence the name _flex_.\n"
    "\n"
    " If the pretty printer decides to add a newline after `flex_break` it will be\n"
    " rendered as its first argument, otherwise as its first argument.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let message =\n"
    "   [from_string(\"pretty\"), from_string(\"printed\"), from_string(\"string\")]\n"
    "   |> join(with: flex_break(\"•\", \"↩\"))\n"
    "   |> group\n"
    "\n"
    " message |> to_string(80)\n"
    " // -> \"pretty•printed•string\"\n"
    "\n"
    " message |> to_string(20)\n"
    " // -> \"pretty•printed↩\n"
    " // string\"\n"
    " ```\n"
).
-spec flex_break(binary(), binary()) -> document().
flex_break(Unbroken, Broken) ->
    {flex_break, Unbroken, Broken}.

-file("src/glam/doc.gleam", 224).
?DOC(
    " Forces the pretty printer to break all the `break`s of the outermost\n"
    " document. This still _has no effect on `group`s_ as the pretty printer will\n"
    " always try to put them on a single line before splitting them.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [from_string(\"pretty\"), break(\"•\", \"↩\"), from_string(\"printed\")]\n"
    " |> concat\n"
    " |> force_break\n"
    " |> group\n"
    " |> to_string(100)\n"
    " // -> \"pretty↩\n"
    " // printed\"\n"
    " ```\n"
).
-spec force_break(document()) -> document().
force_break(Doc) ->
    {force_break, Doc}.

-file("src/glam/doc.gleam", 237).
?DOC(
    " Turns a string into a document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " \"doc\" |> from_string |> to_string(80)\n"
    " // -> \"doc\"\n"
    " ```\n"
).
-spec from_string(binary()) -> document().
from_string(String) ->
    {text, String, gleam@string:length(String)}.

-file("src/glam/doc.gleam", 264).
?DOC(
    " Turns a string into a document whose length is not taken into account when\n"
    " formatting it.\n"
    "\n"
    " This kind of string can be used to render non-visible characters like ansi\n"
    " color codes.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " // Should break in two lines, but doesn't because of the zero_width_string\n"
    " // does not contribute to the total line length.\n"
    " [\n"
    "   zero_width_string(\"\\u{001b}[1;31m\"),\n"
    "   from_string(\"I'm a red\"),\n"
    "   break(\", \", \",\"),\n"
    "   from_string(\"bold text\"),\n"
    " ]\n"
    " |> concat\n"
    " |> group\n"
    " |> to_string(20)\n"
    " // -> \"\\u{001b}[1;31mI'm a red, bold text\"\n"
    " ```\n"
).
-spec zero_width_string(binary()) -> document().
zero_width_string(String) ->
    {text, String, 0}.

-file("src/glam/doc.gleam", 310).
?DOC(
    " Allows the pretty printer to break the `break` documents inside the given\n"
    " group.\n"
    "\n"
    " When the pretty printer runs into a group it first tries to render it on a\n"
    " single line, displaying all the breaks as their first argument.\n"
    " If the group fits this is the final pretty printed result.\n"
    "\n"
    " However, if the group does not fit on a single line _all_ the `break`s\n"
    " inside that group are rendered as their second argument and immediately\n"
    " followed by a newline.\n"
    "\n"
    " Any nested group is considered on its own and may or may not be split,\n"
    " depending if it fits on a single line or not. So, even if the outermost\n"
    " group is broken, its nested groups may still end up on a single line.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let food =\n"
    "   [\"lasagna\", \"ravioli\", \"pizza\"]\n"
    "   |> list.map(from_string) |> join(with: space) |> group\n"
    " let message =\n"
    "   [from_string(\"Food I love:\"), space, food] |> concat |> group\n"
    "\n"
    " message |> to_string(80)\n"
    " // -> \"Food I love: lasagna ravioli pizza\"\n"
    "\n"
    " message |> to_string(30)\n"
    " // -> \"Food I love:\n"
    " // lasagna ravioli pizza\"\n"
    " // ^-- After splitting the outer group, the inner one can fit\n"
    " //     on a single line so the pretty printer does not split it\n"
    "\n"
    " message |> to_string(20)\n"
    " // \"Food I love:\n"
    " // lasagna\n"
    " // ravioli\n"
    " // pizza\"\n"
    " // ^-- Even after splitting the outer group, the inner one wouldn't\n"
    " //     fit on a single line, so the pretty printer splits that as well\n"
    " ```\n"
).
-spec group(document()) -> document().
group(Doc) ->
    {group, Doc}.

-file("src/glam/doc.gleam", 329).
?DOC(
    " Joins a list of documents inserting the given separator between\n"
    " each existing document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let message =\n"
    "   [\"Gleam\", \"is\", \"fun!\"]\n"
    "   |> list.map(from_string)\n"
    "   |> join(with: space)\n"
    "\n"
    " message |> to_string(80)\n"
    " // -> \"Gleam is fun!\"\n"
    " ```\n"
).
-spec join(list(document()), document()) -> document().
join(Docs, Separator) ->
    concat(gleam@list:intersperse(Docs, Separator)).

-file("src/glam/doc.gleam", 124).
?DOC(
    " Joins a list of documents into a single one by inserting the given\n"
    " separators between each existing document.\n"
    "\n"
    " This is a shorthand for `join(docs, concat(separators))`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\"wow\", \"so\", \"many\", \"commas\"]\n"
    " |> list.map(from_string)\n"
    " |> concat_join([from_string(\",\"), space])\n"
    " |> to_string(80)\n"
    " // -> \"wow, so, many, commas\"\n"
    " ```\n"
).
-spec concat_join(list(document()), list(document())) -> document().
concat_join(Docs, Separators) ->
    join(Docs, concat(Separators)).

-file("src/glam/doc.gleam", 353).
?DOC(
    " A document that is always printed as a series of consecutive newlines.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " lines(3) |> to_string(80)\n"
    " // -> \"\\n\\n\\n\"\n"
    " ```\n"
).
-spec lines(integer()) -> document().
lines(Size) ->
    {line, Size}.

-file("src/glam/doc.gleam", 377).
?DOC(
    " Increases the nesting level of a document by the given amount.\n"
    "\n"
    " When the pretty printer breaks a group by inserting a newline, it also adds\n"
    " a whitespace padding equal to its nesting level.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let one = [space, from_string(\"one\")] |> concat |> nest(by: 1)\n"
    " let two = [space, from_string(\"two\")] |> concat |> nest(by: 2)\n"
    " let three = [space, from_string(\"three\")] |> concat |> nest(by: 3)\n"
    " let list = [from_string(\"list:\"), one, two, three] |> concat |> group\n"
    "\n"
    " list |> to_string(10)\n"
    " // -> \"list:\n"
    " //  one\n"
    " //   two\n"
    " //    three\"\n"
    " ```\n"
).
-spec nest(document(), integer()) -> document().
nest(Doc, Indentation) ->
    {nest, Doc, Indentation}.

-file("src/glam/doc.gleam", 400).
?DOC(
    " Joins together a list of documents and increases their nesting level.\n"
    "\n"
    " This is a shorthand for `nest(concat(docs), by: indentation)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [from_string(\"one\"), space, from_string(\"two\")]\n"
    " |> nest_docs(by: 2)\n"
    " |> append(space)\n"
    " |> append(from_string(\"three\"))\n"
    " |> group\n"
    " |> to_string(5)\n"
    " // ->\n"
    " // one\n"
    " //   two\n"
    " // three\n"
    " ```\n"
).
-spec nest_docs(list(document()), integer()) -> document().
nest_docs(Docs, Indentation) ->
    {nest, concat(Docs), Indentation}.

-file("src/glam/doc.gleam", 415).
?DOC(
    " Prefixes a document to another one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_string(\"printed!\")\n"
    " |> prepend(from_string(\"pretty \"))\n"
    " |> to_string(80)\n"
    " // -> \"pretty printed!\"\n"
    " ```\n"
).
-spec prepend(document(), document()) -> document().
prepend(First, Second) ->
    case First of
        {concat, Docs} ->
            {concat, [Second | Docs]};

        _ ->
            {concat, [Second, First]}
    end.

-file("src/glam/doc.gleam", 435).
?DOC(
    " Prefixes multiple documents to another one.\n"
    "\n"
    " This is a shorthand for `prepend(to: first, doc: concat(docs))`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_string(\"fun!\")\n"
    " |> prepend_docs([from_string(\"Gleam \"), from_string(\"is \")])\n"
    " |> to_string(80)\n"
    " // -> \"Gleam is fun!\"\n"
    " ```\n"
).
-spec prepend_docs(document(), list(document())) -> document().
prepend_docs(First, Docs) ->
    prepend(First, concat(Docs)).

-file("src/glam/doc.gleam", 517).
-spec fits(list({integer(), mode(), document()}), integer(), integer()) -> boolean().
fits(Docs, Max_width, Current_width) ->
    case Docs of
        _ when Current_width > Max_width ->
            false;

        [] ->
            true;

        [{Indent, Mode, Doc} | Rest] ->
            case Doc of
                {line, _} ->
                    true;

                {force_break, _} ->
                    false;

                {text, _, Length} ->
                    fits(Rest, Max_width, Current_width + Length);

                {nest, Doc@1, I} ->
                    _pipe = [{Indent + I, Mode, Doc@1} | Rest],
                    fits(_pipe, Max_width, Current_width);

                {break, Unbroken, _} ->
                    case Mode of
                        broken ->
                            true;

                        force_broken ->
                            true;

                        unbroken ->
                            fits(
                                Rest,
                                Max_width,
                                Current_width + gleam@string:length(Unbroken)
                            )
                    end;

                {flex_break, Unbroken, _} ->
                    case Mode of
                        broken ->
                            true;

                        force_broken ->
                            true;

                        unbroken ->
                            fits(
                                Rest,
                                Max_width,
                                Current_width + gleam@string:length(Unbroken)
                            )
                    end;

                {group, Doc@2} ->
                    fits(
                        [{Indent, Mode, Doc@2} | Rest],
                        Max_width,
                        Current_width
                    );

                {concat, Docs@1} ->
                    _pipe@1 = gleam@list:map(
                        Docs@1,
                        fun(Doc@3) -> {Indent, Mode, Doc@3} end
                    ),
                    _pipe@2 = lists:append(_pipe@1, Rest),
                    fits(_pipe@2, Max_width, Current_width)
            end
    end.

-file("src/glam/doc.gleam", 713).
-spec do_flatten(list(document()), list(document())) -> list(document()).
do_flatten(Docs, Acc) ->
    case Docs of
        [] ->
            lists:reverse(Acc);

        [One] ->
            lists:reverse([One | Acc]);

        [{concat, One@1}, {concat, Two} | Rest] ->
            do_flatten([{concat, lists:append(One@1, Two)} | Rest], Acc);

        [{text, One@2, Len_one}, {text, Two@1, Len_two} | Rest@1] ->
            do_flatten(
                [{text, <<One@2/binary, Two@1/binary>>, Len_one + Len_two} |
                    Rest@1],
                Acc
            );

        [One@3, Two@2 | Rest@2] ->
            do_flatten([Two@2 | Rest@2], [One@3 | Acc])
    end.

-file("src/glam/doc.gleam", 709).
-spec flatten(list(document())) -> list(document()).
flatten(Docs) ->
    do_flatten(Docs, []).

-file("src/glam/doc.gleam", 729).
-spec do_split_groups(
    list(document()),
    list(document()),
    list(list(document()))
) -> list(list(document())).
do_split_groups(Docs, Current_group, Acc) ->
    case Docs of
        [] ->
            case Current_group of
                [] ->
                    lists:reverse(Acc);

                _ ->
                    lists:reverse([lists:reverse(Current_group) | Acc])
            end;

        [{group, _} = Doc | Rest] ->
            case Current_group of
                [] ->
                    do_split_groups(Rest, [], [[Doc] | Acc]);

                _ ->
                    do_split_groups(
                        Rest,
                        [],
                        [[Doc], lists:reverse(Current_group) | Acc]
                    )
            end;

        [Doc@1 | Rest@1] ->
            do_split_groups(Rest@1, [Doc@1 | Current_group], Acc)
    end.

-file("src/glam/doc.gleam", 725).
-spec split_groups(list(document())) -> list(list(document())).
split_groups(Docs) ->
    do_split_groups(Docs, [], []).

-file("src/glam/doc.gleam", 751).
-spec superscript_number(integer()) -> binary().
superscript_number(Number) ->
    Digits@1 = case gleam@int:digits(Number, 10) of
        {ok, Digits} -> Digits;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"glam/doc"/utf8>>,
                        function => <<"superscript_number"/utf8>>,
                        line => 752,
                        value => _assert_fail,
                        start => 20397,
                        'end' => 20443,
                        pattern_start => 20408,
                        pattern_end => 20418})
    end,
    gleam@list:fold(
        Digits@1,
        <<""/utf8>>,
        fun(Acc, Digit) ->
            Digit@1 = case Digit of
                0 ->
                    <<"⁰"/utf8>>;

                1 ->
                    <<"¹"/utf8>>;

                2 ->
                    <<"²"/utf8>>;

                3 ->
                    <<"³"/utf8>>;

                4 ->
                    <<"⁴"/utf8>>;

                5 ->
                    <<"⁵"/utf8>>;

                6 ->
                    <<"⁶"/utf8>>;

                7 ->
                    <<"⁷"/utf8>>;

                8 ->
                    <<"⁸"/utf8>>;

                9 ->
                    <<"⁹"/utf8>>;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"not a digit"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"glam/doc"/utf8>>,
                            function => <<"superscript_number"/utf8>>,
                            line => 765})
            end,
            <<Acc/binary, Digit@1/binary>>
        end
    ).

-file("src/glam/doc.gleam", 772).
-spec indentation(integer()) -> binary().
indentation(Size) ->
    gleam@string:repeat(<<" "/utf8>>, Size).

-file("src/glam/doc.gleam", 558).
-spec do_to_string(
    binary(),
    integer(),
    integer(),
    list({integer(), mode(), document()})
) -> binary().
do_to_string(Acc, Max_width, Current_width, Docs) ->
    case Docs of
        [] ->
            Acc;

        [{Indent, Mode, Doc} | Rest] ->
            case Doc of
                {line, Size} ->
                    _pipe = (<<<<Acc/binary,
                            (gleam@string:repeat(<<"\n"/utf8>>, Size))/binary>>/binary,
                        (indentation(Indent))/binary>>),
                    do_to_string(_pipe, Max_width, Indent, Rest);

                {flex_break, Unbroken, Broken} ->
                    New_unbroken_width = Current_width + gleam@string:length(
                        Unbroken
                    ),
                    case fits(Rest, Max_width, New_unbroken_width) of
                        true ->
                            _pipe@1 = (<<Acc/binary, Unbroken/binary>>),
                            do_to_string(
                                _pipe@1,
                                Max_width,
                                New_unbroken_width,
                                Rest
                            );

                        false ->
                            _pipe@2 = (<<<<<<Acc/binary, Broken/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                (indentation(Indent))/binary>>),
                            do_to_string(_pipe@2, Max_width, Indent, Rest)
                    end;

                {break, Unbroken@1, Broken@1} ->
                    case Mode of
                        unbroken ->
                            New_width = Current_width + gleam@string:length(
                                Unbroken@1
                            ),
                            do_to_string(
                                <<Acc/binary, Unbroken@1/binary>>,
                                Max_width,
                                New_width,
                                Rest
                            );

                        broken ->
                            _pipe@3 = (<<<<<<Acc/binary, Broken@1/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                (indentation(Indent))/binary>>),
                            do_to_string(_pipe@3, Max_width, Indent, Rest);

                        force_broken ->
                            _pipe@3 = (<<<<<<Acc/binary, Broken@1/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                (indentation(Indent))/binary>>),
                            do_to_string(_pipe@3, Max_width, Indent, Rest)
                    end;

                {force_break, Doc@1} ->
                    Docs@1 = [{Indent, force_broken, Doc@1} | Rest],
                    do_to_string(Acc, Max_width, Current_width, Docs@1);

                {concat, Docs@2} ->
                    Docs@3 = begin
                        _pipe@4 = gleam@list:map(
                            Docs@2,
                            fun(Doc@2) -> {Indent, Mode, Doc@2} end
                        ),
                        lists:append(_pipe@4, Rest)
                    end,
                    do_to_string(Acc, Max_width, Current_width, Docs@3);

                {group, Doc@3} ->
                    Fits = fits(
                        [{Indent, unbroken, Doc@3}],
                        Max_width,
                        Current_width
                    ),
                    New_mode = case Fits of
                        true ->
                            unbroken;

                        false ->
                            broken
                    end,
                    Docs@4 = [{Indent, New_mode, Doc@3} | Rest],
                    do_to_string(Acc, Max_width, Current_width, Docs@4);

                {nest, Doc@4, I} ->
                    Docs@5 = [{Indent + I, Mode, Doc@4} | Rest],
                    do_to_string(Acc, Max_width, Current_width, Docs@5);

                {text, Text, Length} ->
                    do_to_string(
                        <<Acc/binary, Text/binary>>,
                        Max_width,
                        Current_width + Length,
                        Rest
                    )
            end
    end.

-file("src/glam/doc.gleam", 507).
?DOC(
    " Turns a document into a pretty printed string, trying to fit it into lines\n"
    " of maximum size specified by `limit`.\n"
    "\n"
    " The pretty printed process can be thought of as follows:\n"
    " - the pretty printer first tries to print every group on a single line\n"
    " - all the `break` documents are rendered as their first argument\n"
    " - if the string fits on the specified width this is the result\n"
    " - if the string does not fit on a single line the outermost group is split:\n"
    "   - all of its `break` documents are rendered as their second argument\n"
    "   - a newline is inserted after every `break`\n"
    "   - a padding of the given nesting level is added after every inserted\n"
    "     newline\n"
    "   - all inner groups are then considered on their own: the splitting of the\n"
    "     outermost group does not imply that the inner groups will be split as\n"
    "     well\n"
    "\n"
    " ## Examples\n"
    "\n"
    " For some examples of how pretty printing works for each kind of document you\n"
    " can have a look at the package documentation.\n"
    " There's also a\n"
    " [step-by-step tutorial](https://hexdocs.pm/glam/01_gleam_lists.html)\n"
    " that will guide you through the implementation of a simple pretty printer,\n"
    " covering most of the Glam API.\n"
).
-spec to_string(document(), integer()) -> binary().
to_string(Doc, Limit) ->
    do_to_string(<<""/utf8>>, Limit, 0, [{0, unbroken, Doc}]).

-file("src/glam/doc.gleam", 697).
-spec parenthesise(document(), binary(), binary()) -> document().
parenthesise(Document, Open, Close) ->
    _pipe = [from_string(Open),
        nest({line, 1}, 2),
        nest(Document, 2),
        {line, 1},
        from_string(Close)],
    _pipe@1 = concat(_pipe),
    group(_pipe@1).

-file("src/glam/doc.gleam", 651).
?DOC(
    " Returns a debug version of the given document that can be pretty printed\n"
    " to see the structure of a document.\n"
    "\n"
    " This can help you see how your data structures get turned into documents\n"
    " and check if the document is what you'd expect.\n"
    "\n"
    " - `group`s are surrounded by square brackets.\n"
    " - `nest`s are surrounded by angle brackets and have a smal superscript\n"
    "   with the nesting.\n"
    " - `concat`enated documents are separated by dots.\n"
    " - `break`s are rendered surrounded by curly brackets and show both the\n"
    "   broken and unbroken versions.\n"
    " - `line`s are rendered as the string `lf` followed by a superscript\n"
    "   number of lines.\n"
).
-spec debug(document()) -> document().
debug(Document) ->
    case Document of
        {text, Text, _} ->
            Escaped = gleam@string:replace(Text, <<"\""/utf8>>, <<"\\\""/utf8>>),
            from_string(<<<<"\""/utf8, Escaped/binary>>/binary, "\""/utf8>>);

        {force_break, Doc} ->
            parenthesise(debug(Doc), <<"force("/utf8>>, <<")"/utf8>>);

        {group, Doc@1} ->
            _pipe = parenthesise(debug(Doc@1), <<"["/utf8>>, <<"]"/utf8>>),
            force_break(_pipe);

        {nest, Doc@2, Indentation} ->
            _pipe@1 = parenthesise(
                debug(Doc@2),
                <<(superscript_number(Indentation))/binary, "⟨"/utf8>>,
                <<"⟩"/utf8>>
            ),
            force_break(_pipe@1);

        {break, <<" "/utf8>>, <<""/utf8>>} ->
            from_string(<<"space"/utf8>>);

        {break, Unbroken, Broken} ->
            from_string(
                <<<<<<<<"{ \""/utf8, Unbroken/binary>>/binary, "\", \""/utf8>>/binary,
                        Broken/binary>>/binary,
                    "\" }"/utf8>>
            );

        {flex_break, <<" "/utf8>>, <<""/utf8>>} ->
            from_string(<<"flex_space"/utf8>>);

        {flex_break, Unbroken@1, Broken@1} ->
            from_string(
                <<<<<<<<"flex{ \""/utf8, Unbroken@1/binary>>/binary,
                            "\", \""/utf8>>/binary,
                        Broken@1/binary>>/binary,
                    "\" }"/utf8>>
            );

        {line, Size} ->
            case gleam@int:compare(Size, 1) of
                lt ->
                    from_string(<<"lf"/utf8>>);

                eq ->
                    from_string(<<"lf"/utf8>>);

                gt ->
                    from_string(
                        <<"lf"/utf8, (superscript_number(Size))/binary>>
                    )
            end;

        {concat, Docs} ->
            _pipe@2 = split_groups(flatten(Docs)),
            _pipe@5 = gleam@list:map(
                _pipe@2,
                fun(Docs@1) ->
                    case Docs@1 of
                        [] ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"empty"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"glam/doc"/utf8>>,
                                    function => <<"debug"/utf8>>,
                                    line => 686});

                        _ ->
                            nil
                    end,
                    _pipe@3 = gleam@list:map(Docs@1, fun debug/1),
                    _pipe@4 = join(
                        _pipe@3,
                        flex_break(<<" . "/utf8>>, <<" ."/utf8>>)
                    ),
                    group(_pipe@4)
                end
            ),
            join(
                _pipe@5,
                concat([flex_break(<<" . "/utf8>>, <<" ."/utf8>>), {line, 1}])
            )
    end.
