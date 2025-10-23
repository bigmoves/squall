-module(gleam@bit_array).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/bit_array.gleam").
-export([from_string/1, byte_size/1, slice/3, is_utf8/1, to_string/1, concat/1, append/2, base64_encode/2, base64_decode/1, base64_url_encode/2, base64_url_decode/1, base16_encode/1, base16_decode/1, inspect/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(" BitArrays are a sequence of binary data of any length.\n").

-file("src/gleam/bit_array.gleam", 11).
?DOC(" Converts a UTF-8 `String` type into a `BitArray`.\n").
-spec from_string(binary()) -> bitstring().
from_string(X) ->
    gleam_stdlib:identity(X).

-file("src/gleam/bit_array.gleam", 17).
?DOC(" Returns an integer which is the number of bytes in the bit array.\n").
-spec byte_size(bitstring()) -> integer().
byte_size(X) ->
    erlang:byte_size(X).

-file("src/gleam/bit_array.gleam", 42).
?DOC(
    " Extracts a sub-section of a bit array.\n"
    "\n"
    " The slice will start at given position and continue up to specified\n"
    " length.\n"
    " A negative length can be used to extract bytes at the end of a bit array.\n"
    "\n"
    " This function runs in constant time.\n"
).
-spec slice(bitstring(), integer(), integer()) -> {ok, bitstring()} |
    {error, nil}.
slice(String, Position, Length) ->
    gleam_stdlib:bit_array_slice(String, Position, Length).

-file("src/gleam/bit_array.gleam", 55).
-spec do_is_utf8(bitstring()) -> boolean().
do_is_utf8(Bits) ->
    case Bits of
        <<>> ->
            true;

        <<_/utf8, Rest/binary>> ->
            do_is_utf8(Rest);

        _ ->
            false
    end.

-file("src/gleam/bit_array.gleam", 50).
?DOC(" Tests to see whether a bit array is valid UTF-8.\n").
-spec is_utf8(bitstring()) -> boolean().
is_utf8(Bits) ->
    do_is_utf8(Bits).

-file("src/gleam/bit_array.gleam", 83).
-spec do_to_string(bitstring()) -> {ok, binary()} | {error, nil}.
do_to_string(Bits) ->
    case is_utf8(Bits) of
        true ->
            {ok, gleam_stdlib:identity(Bits)};

        false ->
            {error, nil}
    end.

-file("src/gleam/bit_array.gleam", 75).
?DOC(
    " Converts a bit array to a string.\n"
    "\n"
    " Returns an error if the bit array is invalid UTF-8 data.\n"
).
-spec to_string(bitstring()) -> {ok, binary()} | {error, nil}.
to_string(Bits) ->
    do_to_string(Bits).

-file("src/gleam/bit_array.gleam", 101).
?DOC(
    " Creates a new bit array by joining multiple binaries.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " concat([from_string(\"butter\"), from_string(\"fly\")])\n"
    " // -> from_string(\"butterfly\")\n"
    " ```\n"
).
-spec concat(list(bitstring())) -> bitstring().
concat(Bit_arrays) ->
    gleam_stdlib:bit_array_concat(Bit_arrays).

-file("src/gleam/bit_array.gleam", 28).
?DOC(
    " Creates a new bit array by joining two bit arrays.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " append(to: from_string(\"butter\"), suffix: from_string(\"fly\"))\n"
    " // -> from_string(\"butterfly\")\n"
    " ```\n"
).
-spec append(bitstring(), bitstring()) -> bitstring().
append(First, Second) ->
    gleam_stdlib:bit_array_concat([First, Second]).

-file("src/gleam/bit_array.gleam", 107).
?DOC(" Encodes a BitArray into a base 64 encoded string.\n").
-spec base64_encode(bitstring(), boolean()) -> binary().
base64_encode(Input, Padding) ->
    gleam_stdlib:bit_array_base64_encode(Input, Padding).

-file("src/gleam/bit_array.gleam", 111).
?DOC(" Decodes a base 64 encoded string into a `BitArray`.\n").
-spec base64_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base64_decode(Encoded) ->
    Padded = case erlang:byte_size(gleam_stdlib:identity(Encoded)) rem 4 of
        0 ->
            Encoded;

        N ->
            gleam@string:append(
                Encoded,
                gleam@string:repeat(<<"="/utf8>>, 4 - N)
            )
    end,
    gleam_stdlib:base_decode64(Padded).

-file("src/gleam/bit_array.gleam", 125).
?DOC(" Encodes a `BitArray` into a base 64 encoded string with URL and filename safe alphabet.\n").
-spec base64_url_encode(bitstring(), boolean()) -> binary().
base64_url_encode(Input, Padding) ->
    _pipe = gleam_stdlib:bit_array_base64_encode(Input, Padding),
    _pipe@1 = gleam@string:replace(_pipe, <<"+"/utf8>>, <<"-"/utf8>>),
    gleam@string:replace(_pipe@1, <<"/"/utf8>>, <<"_"/utf8>>).

-file("src/gleam/bit_array.gleam", 133).
?DOC(" Decodes a base 64 encoded string with URL and filename safe alphabet into a `BitArray`.\n").
-spec base64_url_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base64_url_decode(Encoded) ->
    _pipe = Encoded,
    _pipe@1 = gleam@string:replace(_pipe, <<"-"/utf8>>, <<"+"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"_"/utf8>>, <<"/"/utf8>>),
    base64_decode(_pipe@2).

-file("src/gleam/bit_array.gleam", 142).
-spec base16_encode(bitstring()) -> binary().
base16_encode(Input) ->
    binary:encode_hex(Input).

-file("src/gleam/bit_array.gleam", 146).
-spec base16_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base16_decode(Input) ->
    gleam_stdlib:base16_decode(Input).

-file("src/gleam/bit_array.gleam", 170).
-spec do_inspect(bitstring(), binary()) -> binary().
do_inspect(Input, Accumulator) ->
    case Input of
        <<>> ->
            Accumulator;

        <<X:1>> ->
            <<<<Accumulator/binary, (gleam@int:to_string(X))/binary>>/binary,
                ":size(1)"/utf8>>;

        <<X@1:2>> ->
            <<<<Accumulator/binary, (gleam@int:to_string(X@1))/binary>>/binary,
                ":size(2)"/utf8>>;

        <<X@2:3>> ->
            <<<<Accumulator/binary, (gleam@int:to_string(X@2))/binary>>/binary,
                ":size(3)"/utf8>>;

        <<X@3:4>> ->
            <<<<Accumulator/binary, (gleam@int:to_string(X@3))/binary>>/binary,
                ":size(4)"/utf8>>;

        <<X@4:5>> ->
            <<<<Accumulator/binary, (gleam@int:to_string(X@4))/binary>>/binary,
                ":size(5)"/utf8>>;

        <<X@5:6>> ->
            <<<<Accumulator/binary, (gleam@int:to_string(X@5))/binary>>/binary,
                ":size(6)"/utf8>>;

        <<X@6:7>> ->
            <<<<Accumulator/binary, (gleam@int:to_string(X@6))/binary>>/binary,
                ":size(7)"/utf8>>;

        <<X@7, Rest/bitstring>> ->
            Suffix = case Rest of
                <<>> ->
                    <<""/utf8>>;

                _ ->
                    <<", "/utf8>>
            end,
            Accumulator@1 = <<<<Accumulator/binary,
                    (gleam@int:to_string(X@7))/binary>>/binary,
                Suffix/binary>>,
            do_inspect(Rest, Accumulator@1);

        _ ->
            Accumulator
    end.

-file("src/gleam/bit_array.gleam", 165).
?DOC(
    " Converts a bit array to a string containing the decimal value of each byte.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " inspect(<<0, 20, 0x20, 255>>)\n"
    " // -> \"<<0, 20, 32, 255>>\"\n"
    "\n"
    " inspect(<<100, 5:3>>)\n"
    " // -> \"<<100, 5:size(3)>>\"\n"
    " ```\n"
).
-spec inspect(bitstring()) -> binary().
inspect(Input) ->
    <<(do_inspect(Input, <<"<<"/utf8>>))/binary, ">>"/utf8>>.
