-module(gleam@dynamic).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/dynamic.gleam").
-export([from/1, unsafe_coerce/1, dynamic/1, bit_array/1, classify/1, int/1, float/1, bool/1, shallow_list/1, optional/1, any/1, decode1/2, result/2, list/1, string/1, field/2, optional_field/2, element/2, tuple2/2, tuple3/3, tuple4/4, tuple5/5, tuple6/6, dict/2, decode2/3, decode3/4, decode4/5, decode5/6, decode6/7, decode7/8, decode8/9, decode9/10]).
-export_type([dynamic_/0, decode_error/0, unknown_tuple/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type dynamic_() :: any().

-type decode_error() :: {decode_error, binary(), binary(), list(binary())}.

-type unknown_tuple() :: any().

-file("src/gleam/dynamic.gleam", 29).
?DOC(" Converts any Gleam data into `Dynamic` data.\n").
-spec from(any()) -> dynamic_().
from(A) ->
    gleam_stdlib:identity(A).

-file("src/gleam/dynamic.gleam", 38).
-spec unsafe_coerce(dynamic_()) -> any().
unsafe_coerce(A) ->
    gleam_stdlib:identity(A).

-file("src/gleam/dynamic.gleam", 52).
?DOC(
    " Decodes a `Dynamic` value from a `Dynamic` value.\n"
    "\n"
    " This function doesn't seem very useful at first, but it can be convenient\n"
    " when you need to give a decoder function but you don't actually care what\n"
    " the to-decode value is.\n"
).
-spec dynamic(dynamic_()) -> {ok, dynamic_()} | {error, list(decode_error())}.
dynamic(Value) ->
    {ok, Value}.

-file("src/gleam/dynamic.gleam", 73).
?DOC(
    " Checks to see whether a `Dynamic` value is a bit array, and returns that bit\n"
    " array if it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/bit_array\n"
    "\n"
    " bit_array(from(\"Hello\")) == bit_array.from_string(\"Hello\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " bit_array(from(123))\n"
    " // -> Error([DecodeError(expected: \"BitArray\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec bit_array(dynamic_()) -> {ok, bitstring()} | {error, list(decode_error())}.
bit_array(Data) ->
    gleam_stdlib:decode_bit_array(Data).

-file("src/gleam/dynamic.gleam", 120).
-spec put_expected(decode_error(), binary()) -> decode_error().
put_expected(Error, Expected) ->
    {decode_error, Expected, erlang:element(3, Error), erlang:element(4, Error)}.

-file("src/gleam/dynamic.gleam", 131).
?DOC(
    " Return a string indicating the type of the dynamic value.\n"
    "\n"
    " ```gleam\n"
    " classify(from(\"Hello\"))\n"
    " // -> \"String\"\n"
    " ```\n"
).
-spec classify(dynamic_()) -> binary().
classify(Data) ->
    gleam_stdlib:classify_dynamic(Data).

-file("src/gleam/dynamic.gleam", 154).
?DOC(
    " Checks to see whether a `Dynamic` value is an int, and returns that int if it\n"
    " is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " int(from(123))\n"
    " // -> Ok(123)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " int(from(\"Hello\"))\n"
    " // -> Error([DecodeError(expected: \"Int\", found: \"String\", path: [])])\n"
    " ```\n"
).
-spec int(dynamic_()) -> {ok, integer()} | {error, list(decode_error())}.
int(Data) ->
    gleam_stdlib:decode_int(Data).

-file("src/gleam/dynamic.gleam", 177).
?DOC(
    " Checks to see whether a `Dynamic` value is a float, and returns that float if\n"
    " it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " float(from(2.0))\n"
    " // -> Ok(2.0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " float(from(123))\n"
    " // -> Error([DecodeError(expected: \"Float\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec float(dynamic_()) -> {ok, float()} | {error, list(decode_error())}.
float(Data) ->
    gleam_stdlib:decode_float(Data).

-file("src/gleam/dynamic.gleam", 200).
?DOC(
    " Checks to see whether a `Dynamic` value is a bool, and returns that bool if\n"
    " it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " bool(from(True))\n"
    " // -> Ok(True)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " bool(from(123))\n"
    " // -> Error([DecodeError(expected: \"Bool\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec bool(dynamic_()) -> {ok, boolean()} | {error, list(decode_error())}.
bool(Data) ->
    gleam_stdlib:decode_bool(Data).

-file("src/gleam/dynamic.gleam", 226).
?DOC(
    " Checks to see whether a `Dynamic` value is a list, and returns that list if it\n"
    " is. The types of the elements are not checked.\n"
    "\n"
    " If you wish to decode all the elements in the list use the `list` function\n"
    " instead.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " shallow_list(from([\"a\", \"b\", \"c\"]))\n"
    " // -> Ok([from(\"a\"), from(\"b\"), from(\"c\")])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " shallow_list(1)\n"
    " // -> Error([DecodeError(expected: \"List\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec shallow_list(dynamic_()) -> {ok, list(dynamic_())} |
    {error, list(decode_error())}.
shallow_list(Value) ->
    gleam_stdlib:decode_list(Value).

-file("src/gleam/dynamic.gleam", 369).
?DOC(
    " Checks to see if a `Dynamic` value is a nullable version of a particular\n"
    " type, and returns a corresponding `Option` if it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(\"Hello\") |> optional(string)\n"
    " // -> Ok(Some(\"Hello\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"Hello\") |> optional(string)\n"
    " // -> Ok(Some(\"Hello\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " // `gleam/erlang/*` is available via the `gleam_erlang` package\n"
    " import gleam/erlang/atom\n"
    "\n"
    " from(atom.from_string(\"null\")) |> optional(string)\n"
    " // -> Ok(None)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " // `gleam/erlang/*` is available via the `gleam_erlang` package\n"
    " import gleam/erlang/atom\n"
    "\n"
    " from(atom.from_string(\"nil\")) |> optional(string)\n"
    " // -> Ok(None)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " // `gleam/erlang/*` is available via the `gleam_erlang` package\n"
    " import gleam/erlang/atom\n"
    "\n"
    " from(atom.from_string(\"undefined\")) |> optional(string)\n"
    " // -> Ok(None)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(123) |> optional(string)\n"
    " // -> Error([DecodeError(expected: \"String\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec optional(fun((dynamic_()) -> {ok, DLS} | {error, list(decode_error())})) -> fun((dynamic_()) -> {ok,
        gleam@option:option(DLS)} |
    {error, list(decode_error())}).
optional(Decode) ->
    fun(Value) -> gleam_stdlib:decode_option(Value, Decode) end.

-file("src/gleam/dynamic.gleam", 504).
-spec at_least_decode_tuple_error(integer(), dynamic_()) -> {ok, any()} |
    {error, list(decode_error())}.
at_least_decode_tuple_error(Size, Data) ->
    S = case Size of
        1 ->
            <<""/utf8>>;

        _ ->
            <<"s"/utf8>>
    end,
    Error = begin
        _pipe = [<<"Tuple of at least "/utf8>>,
            gleam@int:to_string(Size),
            <<" element"/utf8>>,
            S],
        _pipe@1 = gleam@string_builder:from_strings(_pipe),
        _pipe@2 = gleam@string_builder:to_string(_pipe@1),
        {decode_error, _pipe@2, classify(Data), []}
    end,
    {error, [Error]}.

-file("src/gleam/dynamic.gleam", 1034).
?DOC(
    " Joins multiple decoders into one. When run they will each be tried in turn\n"
    " until one succeeds, or they all fail.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/result\n"
    "\n"
    " let bool_or_string = any(of: [\n"
    "   string,\n"
    "   fn(x) { result.map(bool(x), fn(_) { \"a bool\" }) }\n"
    " ])\n"
    "\n"
    " bool_or_string(from(\"ok\"))\n"
    " // -> Ok(\"ok\")\n"
    "\n"
    " bool_or_string(from(True))\n"
    " // -> Ok(\"a bool\")\n"
    "\n"
    " bool_or_string(from(1))\n"
    " // -> Error(DecodeError(expected: \"another type\", found: \"Int\", path: []))\n"
    " ```\n"
).
-spec any(list(fun((dynamic_()) -> {ok, DPS} | {error, list(decode_error())}))) -> fun((dynamic_()) -> {ok,
        DPS} |
    {error, list(decode_error())}).
any(Decoders) ->
    fun(Data) -> case Decoders of
            [] ->
                {error,
                    [{decode_error, <<"another type"/utf8>>, classify(Data), []}]};

            [Decoder | Decoders@1] ->
                case Decoder(Data) of
                    {ok, Decoded} ->
                        {ok, Decoded};

                    {error, _} ->
                        (any(Decoders@1))(Data)
                end
        end end.

-file("src/gleam/dynamic.gleam", 1530).
-spec all_errors({ok, any()} | {error, list(decode_error())}) -> list(decode_error()).
all_errors(Result) ->
    case Result of
        {ok, _} ->
            [];

        {error, Errors} ->
            Errors
    end.

-file("src/gleam/dynamic.gleam", 1067).
?DOC(
    " Decode 1 value from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0, \"3\")) |> decode1(MyRecord, element(0, int))\n"
    " // -> Ok(MyRecord(1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\")) |> decode1(MyRecord, element(0, int))\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode1(
    fun((DPW) -> DPX),
    fun((dynamic_()) -> {ok, DPW} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DPX} | {error, list(decode_error())}).
decode1(Constructor, T1) ->
    fun(Value) -> case T1(Value) of
            {ok, A} ->
                {ok, Constructor(A)};

            A@1 ->
                {error, all_errors(A@1)}
        end end.

-file("src/gleam/dynamic.gleam", 576).
-spec push_path(decode_error(), any()) -> decode_error().
push_path(Error, Name) ->
    Name@1 = from(Name),
    Decoder = any(
        [fun string/1,
            fun(X) -> gleam@result:map(int(X), fun gleam@int:to_string/1) end]
    ),
    Name@3 = case Decoder(Name@1) of
        {ok, Name@2} ->
            Name@2;

        {error, _} ->
            _pipe = [<<"<"/utf8>>, classify(Name@1), <<">"/utf8>>],
            _pipe@1 = gleam@string_builder:from_strings(_pipe),
            gleam@string_builder:to_string(_pipe@1)
    end,
    {decode_error,
        erlang:element(2, Error),
        erlang:element(3, Error),
        [Name@3 | erlang:element(4, Error)]}.

-file("src/gleam/dynamic.gleam", 261).
?DOC(
    " Checks to see whether a `Dynamic` value is a result of a particular type, and\n"
    " returns that result if it is.\n"
    "\n"
    " The `ok` and `error` arguments are decoders for decoding the `Ok` and\n"
    " `Error` values of the result.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(Ok(1)) |> result(ok: int, error: string)\n"
    " // -> Ok(Ok(1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(Error(\"boom\")) |> result(ok: int, error: string)\n"
    " // -> Ok(Error(\"boom\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(123) |> result(ok: int, error: string)\n"
    " // -> Error([DecodeError(expected: \"Result\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec result(
    fun((dynamic_()) -> {ok, DLG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DLI} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {ok, DLG} | {error, DLI}} |
    {error, list(decode_error())}).
result(Decode_ok, Decode_error) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_result(Value),
            fun(Inner_result) -> case Inner_result of
                    {ok, Raw} ->
                        gleam@result:'try'(
                            begin
                                _pipe = Decode_ok(Raw),
                                map_errors(
                                    _pipe,
                                    fun(_capture) ->
                                        push_path(_capture, <<"ok"/utf8>>)
                                    end
                                )
                            end,
                            fun(Value@1) -> {ok, {ok, Value@1}} end
                        );

                    {error, Raw@1} ->
                        gleam@result:'try'(
                            begin
                                _pipe@1 = Decode_error(Raw@1),
                                map_errors(
                                    _pipe@1,
                                    fun(_capture@1) ->
                                        push_path(_capture@1, <<"error"/utf8>>)
                                    end
                                )
                            end,
                            fun(Value@2) -> {ok, {error, Value@2}} end
                        )
                end end
        )
    end.

-file("src/gleam/dynamic.gleam", 314).
?DOC(
    " Checks to see whether a `Dynamic` value is a list of a particular type, and\n"
    " returns that list if it is.\n"
    "\n"
    " The second argument is a decoder function used to decode the elements of\n"
    " the list. The list is only decoded if all elements in the list can be\n"
    " successfully decoded using this function.\n"
    "\n"
    " If you do not wish to decode all the elements in the list use the `shallow_list`\n"
    " function instead.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from([\"a\", \"b\", \"c\"]) |> list(of: string)\n"
    " // -> Ok([\"a\", \"b\", \"c\"])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([1, 2, 3]) |> list(of: string)\n"
    " // -> Error([DecodeError(expected: \"String\", found: \"Int\", path: [\"*\"])])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"ok\") |> list(of: string)\n"
    " // -> Error([DecodeError(expected: \"List\", found: \"String\", path: [])])\n"
    " ```\n"
).
-spec list(fun((dynamic_()) -> {ok, DLN} | {error, list(decode_error())})) -> fun((dynamic_()) -> {ok,
        list(DLN)} |
    {error, list(decode_error())}).
list(Decoder_type) ->
    fun(Dynamic) ->
        gleam@result:'try'(shallow_list(Dynamic), fun(List) -> _pipe = List,
                _pipe@1 = gleam@list:try_map(_pipe, Decoder_type),
                map_errors(
                    _pipe@1,
                    fun(_capture) -> push_path(_capture, <<"*"/utf8>>) end
                ) end)
    end.

-file("src/gleam/dynamic.gleam", 100).
-spec map_errors(
    {ok, DKB} | {error, list(decode_error())},
    fun((decode_error()) -> decode_error())
) -> {ok, DKB} | {error, list(decode_error())}.
map_errors(Result, F) ->
    gleam@result:map_error(
        Result,
        fun(_capture) -> gleam@list:map(_capture, F) end
    ).

-file("src/gleam/dynamic.gleam", 108).
-spec decode_string(dynamic_()) -> {ok, binary()} |
    {error, list(decode_error())}.
decode_string(Data) ->
    _pipe = bit_array(Data),
    _pipe@1 = map_errors(
        _pipe,
        fun(_capture) -> put_expected(_capture, <<"String"/utf8>>) end
    ),
    gleam@result:'try'(
        _pipe@1,
        fun(Raw) -> case gleam@bit_array:to_string(Raw) of
                {ok, String} ->
                    {ok, String};

                {error, nil} ->
                    {error,
                        [{decode_error,
                                <<"String"/utf8>>,
                                <<"BitArray"/utf8>>,
                                []}]}
            end end
    ).

-file("src/gleam/dynamic.gleam", 96).
?DOC(
    " Checks to see whether a `Dynamic` value is a string, and returns that string if\n"
    " it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " string(from(\"Hello\"))\n"
    " // -> Ok(\"Hello\")\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " string(from(123))\n"
    " // -> Error([DecodeError(expected: \"String\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec string(dynamic_()) -> {ok, binary()} | {error, list(decode_error())}.
string(Data) ->
    decode_string(Data).

-file("src/gleam/dynamic.gleam", 398).
?DOC(
    " Checks to see if a `Dynamic` value is a map with a specific field, and returns\n"
    " the value of that field if it is.\n"
    "\n"
    " This will not succeed on a record.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    " dict.new()\n"
    " |> dict.insert(\"Hello\", \"World\")\n"
    " |> from\n"
    " |> field(named: \"Hello\", of: string)\n"
    " // -> Ok(\"World\")\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(123) |> field(\"Hello\", string)\n"
    " // -> Error([DecodeError(expected: \"Map\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec field(
    any(),
    fun((dynamic_()) -> {ok, DMC} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DMC} | {error, list(decode_error())}).
field(Name, Inner_type) ->
    fun(Value) ->
        Missing_field_error = {decode_error,
            <<"field"/utf8>>,
            <<"nothing"/utf8>>,
            []},
        gleam@result:'try'(
            gleam_stdlib:decode_field(Value, Name),
            fun(Maybe_inner) -> _pipe = Maybe_inner,
                _pipe@1 = gleam@option:to_result(_pipe, [Missing_field_error]),
                _pipe@2 = gleam@result:'try'(_pipe@1, Inner_type),
                map_errors(
                    _pipe@2,
                    fun(_capture) -> push_path(_capture, Name) end
                ) end
        )
    end.

-file("src/gleam/dynamic.gleam", 440).
?DOC(
    " Checks to see if a `Dynamic` value is a map with a specific field.\n"
    " If the map does not have the specified field, returns an `Ok(None)` instead of failing; otherwise,\n"
    " returns the decoded field wrapped in `Some(_)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    " dict.new()\n"
    " |> dict.insert(\"Hello\", \"World\")\n"
    " |> from\n"
    " |> optional_field(named: \"Hello\", of: string)\n"
    " // -> Ok(Some(\"World\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    " dict.new()\n"
    " |> from\n"
    " |> optional_field(named: \"Hello\", of: string)\n"
    " // -> Ok(None)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(123)\n"
    " |> optional_field(\"Hello\", string)\n"
    " // -> Error([DecodeError(expected: \"Map\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec optional_field(
    any(),
    fun((dynamic_()) -> {ok, DMG} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, gleam@option:option(DMG)} |
    {error, list(decode_error())}).
optional_field(Name, Inner_type) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_field(Value, Name),
            fun(Maybe_inner) -> case Maybe_inner of
                    none ->
                        {ok, none};

                    {some, Dynamic_inner} ->
                        _pipe = Dynamic_inner,
                        _pipe@1 = gleam_stdlib:decode_option(_pipe, Inner_type),
                        map_errors(
                            _pipe@1,
                            fun(_capture) -> push_path(_capture, Name) end
                        )
                end end
        )
    end.

-file("src/gleam/dynamic.gleam", 483).
?DOC(
    " Checks to see if a `Dynamic` value is a tuple large enough to have a certain\n"
    " index, and returns the value of that index if it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2))\n"
    " |> element(0, int)\n"
    " // -> Ok(from(1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2))\n"
    " |> element(2, int)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of at least 3 elements\",\n"
    " //     found: \"Tuple of 2 elements\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
).
-spec element(
    integer(),
    fun((dynamic_()) -> {ok, DMO} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DMO} | {error, list(decode_error())}).
element(Index, Inner_type) ->
    fun(Data) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple(Data),
            fun(Tuple) ->
                Size = gleam_stdlib:size_of_tuple(Tuple),
                gleam@result:'try'(case Index >= 0 of
                        true ->
                            case Index < Size of
                                true ->
                                    gleam_stdlib:tuple_get(Tuple, Index);

                                false ->
                                    at_least_decode_tuple_error(Index + 1, Data)
                            end;

                        false ->
                            case gleam@int:absolute_value(Index) =< Size of
                                true ->
                                    gleam_stdlib:tuple_get(Tuple, Size + Index);

                                false ->
                                    at_least_decode_tuple_error(
                                        gleam@int:absolute_value(Index),
                                        Data
                                    )
                            end
                    end, fun(Data@1) -> _pipe = Inner_type(Data@1),
                        map_errors(
                            _pipe,
                            fun(_capture) -> push_path(_capture, Index) end
                        ) end)
            end
        )
    end.

-file("src/gleam/dynamic.gleam", 566).
-spec tuple_errors({ok, any()} | {error, list(decode_error())}, binary()) -> list(decode_error()).
tuple_errors(Result, Name) ->
    case Result of
        {ok, _} ->
            [];

        {error, Errors} ->
            gleam@list:map(
                Errors,
                fun(_capture) -> push_path(_capture, Name) end
            )
    end.

-file("src/gleam/dynamic.gleam", 642).
?DOC(
    " Checks to see if a `Dynamic` value is a 2-element tuple, list or array containing\n"
    " specifically typed elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2))\n"
    " |> tuple2(int, int)\n"
    " // -> Ok(#(1, 2))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0))\n"
    " |> tuple2(int, float)\n"
    " // -> Ok(#(1, 2.0))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([1, 2])\n"
    " |> tuple2(int, int)\n"
    " // -> Ok(#(1, 2))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([from(1), from(2.0)])\n"
    " |> tuple2(int, float)\n"
    " // -> Ok(#(1, 2.0))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2, 3))\n"
    " |> tuple2(int, float)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 2 elements\",\n"
    " //     found: \"Tuple of 3 elements\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"\")\n"
    " |> tuple2(int, float)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 2 elements\",\n"
    " //     found: \"String\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
).
-spec tuple2(
    fun((dynamic_()) -> {ok, DNO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DNQ} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DNO, DNQ}} | {error, list(decode_error())}).
tuple2(Decode1, Decode2) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple2(Value),
            fun(_use0) ->
                {A, B} = _use0,
                case {Decode1(A), Decode2(B)} of
                    {{ok, A@1}, {ok, B@1}} ->
                        {ok, {A@1, B@1}};

                    {A@2, B@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        {error, _pipe@1}
                end
            end
        )
    end.

-file("src/gleam/dynamic.gleam", 711).
?DOC(
    " Checks to see if a `Dynamic` value is a 3-element tuple, list or array containing\n"
    " specifically typed elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2, 3))\n"
    " |> tuple3(int, int, int)\n"
    " // -> Ok(#(1, 2, 3))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0, \"3\"))\n"
    " |> tuple3(int, float, string)\n"
    " // -> Ok(#(1, 2.0, \"3\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([1, 2, 3])\n"
    " |> tuple3(int, int, int)\n"
    " // -> Ok(#(1, 2, 3))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([from(1), from(2.0), from(\"3\")])\n"
    " |> tuple3(int, float, string)\n"
    " // -> Ok(#(1, 2.0, \"3\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2))\n"
    " |> tuple3(int, float, string)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 3 elements\",\n"
    " //     found: \"Tuple of 2 elements\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"\")\n"
    " |> tuple3(int, float, string)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 3 elements\",\n"
    " //     found: \"String\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
).
-spec tuple3(
    fun((dynamic_()) -> {ok, DNT} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DNV} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DNX} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DNT, DNV, DNX}} | {error, list(decode_error())}).
tuple3(Decode1, Decode2, Decode3) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple3(Value),
            fun(_use0) ->
                {A, B, C} = _use0,
                case {Decode1(A), Decode2(B), Decode3(C)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}} ->
                        {ok, {A@1, B@1, C@1}};

                    {A@2, B@2, C@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        {error, _pipe@2}
                end
            end
        )
    end.

-file("src/gleam/dynamic.gleam", 782).
?DOC(
    " Checks to see if a `Dynamic` value is a 4-element tuple, list or array containing\n"
    " specifically typed elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2, 3, 4))\n"
    " |> tuple4(int, int, int, int)\n"
    " // -> Ok(#(1, 2, 3, 4))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0, \"3\", 4))\n"
    " |> tuple4(int, float, string, int)\n"
    " // -> Ok(#(1, 2.0, \"3\", 4))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([1, 2, 3, 4])\n"
    " |> tuple4(int, int, int, int)\n"
    " // -> Ok(#(1, 2, 3, 4))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([from(1), from(2.0), from(\"3\"), from(4)])\n"
    " |> tuple4(int, float, string, int)\n"
    " // -> Ok(#(1, 2.0, \"3\", 4))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2))\n"
    " |> tuple4(int, float, string, int)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 4 elements\",\n"
    " //     found: \"Tuple of 2 elements\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"\")\n"
    " |> tuple4(int, float, string, int)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 4 elements\",\n"
    " //     found: \"String\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
).
-spec tuple4(
    fun((dynamic_()) -> {ok, DOA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOE} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOG} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DOA, DOC, DOE, DOG}} |
    {error, list(decode_error())}).
tuple4(Decode1, Decode2, Decode3, Decode4) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple4(Value),
            fun(_use0) ->
                {A, B, C, D} = _use0,
                case {Decode1(A), Decode2(B), Decode3(C), Decode4(D)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}} ->
                        {ok, {A@1, B@1, C@1, D@1}};

                    {A@2, B@2, C@2, D@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = lists:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        {error, _pipe@3}
                end
            end
        )
    end.

-file("src/gleam/dynamic.gleam", 855).
?DOC(
    " Checks to see if a `Dynamic` value is a 5-element tuple, list or array containing\n"
    " specifically typed elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2, 3, 4, 5))\n"
    " |> tuple5(int, int, int, int, int)\n"
    " // -> Ok(#(1, 2, 3, 4, 5))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0, \"3\", 4, 5))\n"
    " |> tuple5(int, float, string, int, int)\n"
    " // -> Ok(#(1, 2.0, \"3\", 4, 5))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([1, 2, 3, 4, 5])\n"
    " |> tuple5(int, int, int, int, int)\n"
    " // -> Ok(#(1, 2, 3, 4, 5))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([from(1), from(2.0), from(\"3\"), from(4), from(True)])\n"
    " |> tuple5(int, float, string, int, bool)\n"
    " // -> Ok(#(1, 2.0, \"3\", 4, True))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2))\n"
    " |> tuple5(int, float, string, int, int)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 5 elements\",\n"
    " //     found: \"Tuple of 2 elements\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"\")\n"
    " |> tuple5(int, float, string, int, int)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 5 elements\",\n"
    " //     found: \"String\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
).
-spec tuple5(
    fun((dynamic_()) -> {ok, DOJ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOL} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DON} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOP} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOR} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DOJ, DOL, DON, DOP, DOR}} |
    {error, list(decode_error())}).
tuple5(Decode1, Decode2, Decode3, Decode4, Decode5) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple5(Value),
            fun(_use0) ->
                {A, B, C, D, E} = _use0,
                case {Decode1(A),
                    Decode2(B),
                    Decode3(C),
                    Decode4(D),
                    Decode5(E)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}, {ok, E@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1}};

                    {A@2, B@2, C@2, D@2, E@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = lists:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = lists:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        {error, _pipe@4}
                end
            end
        )
    end.

-file("src/gleam/dynamic.gleam", 930).
?DOC(
    " Checks to see if a `Dynamic` value is a 6-element tuple, list or array containing\n"
    " specifically typed elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2, 3, 4, 5, 6))\n"
    " |> tuple6(int, int, int, int, int, int)\n"
    " // -> Ok(#(1, 2, 3, 4, 5, 6))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0, \"3\", 4, 5, 6))\n"
    " |> tuple6(int, float, string, int, int, int)\n"
    " // -> Ok(#(1, 2.0, \"3\", 4, 5, 6))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([1, 2, 3, 4, 5, 6])\n"
    " |> tuple6(int, int, int, int, int, int)\n"
    " // -> Ok(#(1, 2, 3, 4, 5, 6))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from([from(1), from(2.0), from(\"3\"), from(4), from(True), from(False)])\n"
    " |> tuple6(int, float, string, int, bool, bool)\n"
    " // -> Ok(#(1, 2.0, \"3\", 4, True, False))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2))\n"
    " |> tuple6(int, float, string, int, int, int)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 6 elements\",\n"
    " //     found: \"Tuple of 2 elements\",\n"
    " //     path: [],\n"
    " //   ),\n"
    " // ])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"\")\n"
    " |> tuple6(int, float, string, int, int, int)\n"
    " // -> Error([\n"
    " //   DecodeError(\n"
    " //     expected: \"Tuple of 6 elements\",\n"
    " //     found: \"String\",\n"
    " //     path: [],\n"
    " //  ),\n"
    " // ])\n"
    " ```\n"
).
-spec tuple6(
    fun((dynamic_()) -> {ok, DOU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOW} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DOY} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPE} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {DOU, DOW, DOY, DPA, DPC, DPE}} |
    {error, list(decode_error())}).
tuple6(Decode1, Decode2, Decode3, Decode4, Decode5, Decode6) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple6(Value),
            fun(_use0) ->
                {A, B, C, D, E, F} = _use0,
                case {Decode1(A),
                    Decode2(B),
                    Decode3(C),
                    Decode4(D),
                    Decode5(E),
                    Decode6(F)} of
                    {{ok, A@1},
                        {ok, B@1},
                        {ok, C@1},
                        {ok, D@1},
                        {ok, E@1},
                        {ok, F@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1, F@1}};

                    {A@2, B@2, C@2, D@2, E@2, F@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = lists:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = lists:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        _pipe@5 = lists:append(
                            _pipe@4,
                            tuple_errors(F@2, <<"5"/utf8>>)
                        ),
                        {error, _pipe@5}
                end
            end
        )
    end.

-file("src/gleam/dynamic.gleam", 981).
?DOC(
    " Checks to see if a `Dynamic` value is a dict.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    " dict.new() |> from |> dict(string, int)\n"
    " // -> Ok(dict.new())\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(1) |> dict(string, int)\n"
    " // -> Error(DecodeError(expected: \"Map\", found: \"Int\", path: []))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(\"\") |> dict(string, int)\n"
    " // -> Error(DecodeError(expected: \"Map\", found: \"String\", path: []))\n"
    " ```\n"
).
-spec dict(
    fun((dynamic_()) -> {ok, DPH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DPJ} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, gleam@dict:dict(DPH, DPJ)} |
    {error, list(decode_error())}).
dict(Key_type, Value_type) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_map(Value),
            fun(Map) ->
                gleam@result:'try'(
                    begin
                        _pipe = Map,
                        _pipe@1 = maps:to_list(_pipe),
                        gleam@list:try_map(
                            _pipe@1,
                            fun(Pair) ->
                                {K, V} = Pair,
                                gleam@result:'try'(
                                    begin
                                        _pipe@2 = Key_type(K),
                                        map_errors(
                                            _pipe@2,
                                            fun(_capture) ->
                                                push_path(
                                                    _capture,
                                                    <<"keys"/utf8>>
                                                )
                                            end
                                        )
                                    end,
                                    fun(K@1) ->
                                        gleam@result:'try'(
                                            begin
                                                _pipe@3 = Value_type(V),
                                                map_errors(
                                                    _pipe@3,
                                                    fun(_capture@1) ->
                                                        push_path(
                                                            _capture@1,
                                                            <<"values"/utf8>>
                                                        )
                                                    end
                                                )
                                            end,
                                            fun(V@1) -> {ok, {K@1, V@1}} end
                                        )
                                    end
                                )
                            end
                        )
                    end,
                    fun(Pairs) -> {ok, maps:from_list(Pairs)} end
                )
            end
        )
    end.

-file("src/gleam/dynamic.gleam", 1095).
?DOC(
    " Decode 2 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0, \"3\"))\n"
    " |> decode2(MyRecord, element(0, int), element(1, float))\n"
    " // -> Ok(MyRecord(1, 2.0))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\"))\n"
    " |> decode2(MyRecord, element(0, int), element(1, float))\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode2(
    fun((DQA, DQB) -> DQC),
    fun((dynamic_()) -> {ok, DQA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQB} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DQC} | {error, list(decode_error())}).
decode2(Constructor, T1, T2) ->
    fun(Value) -> case {T1(Value), T2(Value)} of
            {{ok, A}, {ok, B}} ->
                {ok, Constructor(A, B)};

            {A@1, B@1} ->
                {error, gleam@list:concat([all_errors(A@1), all_errors(B@1)])}
        end end.

-file("src/gleam/dynamic.gleam", 1127).
?DOC(
    " Decode 3 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.0, \"3\"))\n"
    " |> decode3(MyRecord, element(0, int), element(1, float), element(2, string))\n"
    " // -> Ok(MyRecord(1, 2.0, \"3\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\"))\n"
    " |> decode3(MyRecord, element(0, int), element(1, float), element(2, string))\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode3(
    fun((DQG, DQH, DQI) -> DQJ),
    fun((dynamic_()) -> {ok, DQG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQI} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DQJ} | {error, list(decode_error())}).
decode3(Constructor, T1, T2, T3) ->
    fun(Value) -> case {T1(Value), T2(Value), T3(Value)} of
            {{ok, A}, {ok, B}, {ok, C}} ->
                {ok, Constructor(A, B, C)};

            {A@1, B@1, C@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1), all_errors(B@1), all_errors(C@1)]
                    )}
        end end.

-file("src/gleam/dynamic.gleam", 1173).
?DOC(
    " Decode 4 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.1, \"3\", \"4\"))\n"
    " |> decode4(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    " )\n"
    " // -> Ok(MyRecord(1, 2.1, \"3\", \"4\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\", \"\"))\n"
    " |> decode4(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    " )\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode4(
    fun((DQO, DQP, DQQ, DQR) -> DQS),
    fun((dynamic_()) -> {ok, DQO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQP} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQR} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DQS} | {error, list(decode_error())}).
decode4(Constructor, T1, T2, T3, T4) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}} ->
                {ok, Constructor(A, B, C, D)};

            {A@1, B@1, C@1, D@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1)]
                    )}
        end end.

-file("src/gleam/dynamic.gleam", 1229).
?DOC(
    " Decode 5 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.1, \"3\", \"4\", \"5\"))\n"
    " |> decode5(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    " )\n"
    " // -> Ok(MyRecord(1, 2.1, \"3\", \"4\", \"5\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\", \"\", \"\"))\n"
    " |> decode5(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    " )\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode5(
    fun((DQY, DQZ, DRA, DRB, DRC) -> DRD),
    fun((dynamic_()) -> {ok, DQY} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DQZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRB} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRC} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DRD} | {error, list(decode_error())}).
decode5(Constructor, T1, T2, T3, T4, T5) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}} ->
                {ok, Constructor(A, B, C, D, E)};

            {A@1, B@1, C@1, D@1, E@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1)]
                    )}
        end end.

-file("src/gleam/dynamic.gleam", 1289).
?DOC(
    " Decode 6 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.1, \"3\", \"4\", \"5\", \"6\"))\n"
    " |> decode6(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    " )\n"
    " // -> Ok(MyRecord(1, 2.1, \"3\", \"4\", \"5\", \"6\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\", \"\", \"\", \"\"))\n"
    " |> decode6(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    " )\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode6(
    fun((DRK, DRL, DRM, DRN, DRO, DRP) -> DRQ),
    fun((dynamic_()) -> {ok, DRK} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRL} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRM} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRN} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRP} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DRQ} | {error, list(decode_error())}).
decode6(Constructor, T1, T2, T3, T4, T5, T6) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}} ->
                {ok, Constructor(A, B, C, D, E, F)};

            {A@1, B@1, C@1, D@1, E@1, F@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1)]
                    )}
        end end.

-file("src/gleam/dynamic.gleam", 1354).
?DOC(
    " Decode 7 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.1, \"3\", \"4\", \"5\", \"6\"))\n"
    " |> decode7(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    "   element(6, string),\n"
    " )\n"
    " // -> Ok(MyRecord(1, 2.1, \"3\", \"4\", \"5\", \"6\", \"7\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\", \"\", \"\", \"\", \"\"))\n"
    " |> decode7(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    "   element(6, string),\n"
    " )\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode7(
    fun((DRY, DRZ, DSA, DSB, DSC, DSD, DSE) -> DSF),
    fun((dynamic_()) -> {ok, DRY} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DRZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSB} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSD} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSE} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DSF} | {error, list(decode_error())}).
decode7(Constructor, T1, T2, T3, T4, T5, T6, T7) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}, {ok, G}} ->
                {ok, Constructor(A, B, C, D, E, F, G)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1)]
                    )}
        end end.

-file("src/gleam/dynamic.gleam", 1423).
?DOC(
    " Decode 8 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.1, \"3\", \"4\", \"5\", \"6\", \"7\", \"8\"))\n"
    " |> decode8(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    "   element(6, string),\n"
    "   element(7, string),\n"
    " )\n"
    " // -> Ok(MyRecord(1, 2.1, \"3\", \"4\", \"5\", \"6\", \"7\", \"8\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\"))\n"
    " |> decode8(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    "   element(6, string),\n"
    "   element(7, string),\n"
    " )\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode8(
    fun((DSO, DSP, DSQ, DSR, DSS, DST, DSU, DSV) -> DSW),
    fun((dynamic_()) -> {ok, DSO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSP} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSR} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DST} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DSV} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DSW} | {error, list(decode_error())}).
decode8(Constructor, T1, T2, T3, T4, T5, T6, T7, T8) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X)} of
            {{ok, A},
                {ok, B},
                {ok, C},
                {ok, D},
                {ok, E},
                {ok, F},
                {ok, G},
                {ok, H}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1),
                            all_errors(H@1)]
                    )}
        end end.

-file("src/gleam/dynamic.gleam", 1496).
?DOC(
    " Decode 9 values from a `Dynamic` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from(#(1, 2.1, \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\"))\n"
    " |> decode9(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    "   element(6, string),\n"
    "   element(7, string),\n"
    "   element(8, string),\n"
    " )\n"
    " // -> Ok(MyRecord(1, 2.1, \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from(#(\"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\"))\n"
    " |> decode9(\n"
    "   MyRecord,\n"
    "   element(0, int),\n"
    "   element(1, float),\n"
    "   element(2, string),\n"
    "   element(3, string),\n"
    "   element(4, string),\n"
    "   element(5, string),\n"
    "   element(6, string),\n"
    "   element(7, string),\n"
    "   element(8, string),\n"
    " )\n"
    " // -> Error([\n"
    " //   DecodeError(expected: \"Int\", found: \"String\", path: [\"0\"]),\n"
    " //   DecodeError(expected: \"Float\", found: \"String\", path: [\"1\"]),\n"
    " // ])\n"
    " ```\n"
).
-spec decode9(
    fun((DTG, DTH, DTI, DTJ, DTK, DTL, DTM, DTN, DTO) -> DTP),
    fun((dynamic_()) -> {ok, DTG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTI} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTJ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTK} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTL} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTM} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTN} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, DTO} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, DTP} | {error, list(decode_error())}).
decode9(Constructor, T1, T2, T3, T4, T5, T6, T7, T8, T9) ->
    fun(X) ->
        case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X), T9(X)} of
            {{ok, A},
                {ok, B},
                {ok, C},
                {ok, D},
                {ok, E},
                {ok, F},
                {ok, G},
                {ok, H},
                {ok, I}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H, I)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1, I@1} ->
                {error,
                    gleam@list:concat(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1),
                            all_errors(H@1),
                            all_errors(I@1)]
                    )}
        end
    end.
