-module(gleam@bytes_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/bytes_builder.gleam").
-export([append_builder/2, prepend_builder/2, concat/1, new/0, from_string/1, prepend_string/2, append_string/2, from_string_builder/1, from_bit_array/1, prepend/2, append/2, concat_bit_arrays/1, to_bit_array/1, byte_size/1]).
-export_type([bytes_builder/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " `BytesBuilder` is a type used for efficiently building text content to be\n"
    " written to a file or a socket. Internally it is represented as tree so to\n"
    " append or prepend to a bytes builder is a constant time operation that\n"
    " allocates a new node in the tree without copying any of the content. When\n"
    " writing to an output stream the tree is traversed and the content is sent\n"
    " directly rather than copying it into a single buffer beforehand.\n"
    "\n"
    " If we append one bit array to another the bit arrays must be copied to a\n"
    " new location in memory so that they can sit together. This behaviour\n"
    " enables efficient reading of the data but copying can be expensive,\n"
    " especially if we want to join many bit arrays together.\n"
    "\n"
    " BytesBuilder is different in that it can be joined together in constant\n"
    " time using minimal memory, and then can be efficiently converted to a\n"
    " bit array using the `to_bit_array` function.\n"
    "\n"
    " Byte builders are always byte aligned, so that a number of bits that is not\n"
    " divisible by 8 will be padded with 0s.\n"
    "\n"
    " On Erlang this type is compatible with Erlang's iolists.\n"
).

-opaque bytes_builder() :: {bytes, bitstring()} |
    {text, gleam@string_builder:string_builder()} |
    {many, list(bytes_builder())}.

-file("src/gleam/bytes_builder.gleam", 72).
?DOC(
    " Appends a builder onto the end of another.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec append_builder(bytes_builder(), bytes_builder()) -> bytes_builder().
append_builder(First, Second) ->
    gleam_stdlib:iodata_append(First, Second).

-file("src/gleam/bytes_builder.gleam", 60).
?DOC(
    " Prepends a builder onto the start of another.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec prepend_builder(bytes_builder(), bytes_builder()) -> bytes_builder().
prepend_builder(Second, First) ->
    gleam_stdlib:iodata_append(First, Second).

-file("src/gleam/bytes_builder.gleam", 111).
?DOC(
    " Joins a list of builders into a single builder.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec concat(list(bytes_builder())) -> bytes_builder().
concat(Builders) ->
    gleam_stdlib:identity(Builders).

-file("src/gleam/bytes_builder.gleam", 36).
?DOC(
    " Create an empty `BytesBuilder`. Useful as the start of a pipe chaining many\n"
    " builders together.\n"
).
-spec new() -> bytes_builder().
new() ->
    gleam_stdlib:identity([]).

-file("src/gleam/bytes_builder.gleam", 132).
?DOC(
    " Creates a new builder from a string.\n"
    "\n"
    " Runs in constant time when running on Erlang.\n"
    " Runs in linear time otherwise.\n"
).
-spec from_string(binary()) -> bytes_builder().
from_string(String) ->
    gleam_stdlib:wrap_list(String).

-file("src/gleam/bytes_builder.gleam", 87).
?DOC(
    " Prepends a string onto the start of a builder.\n"
    "\n"
    " Runs in constant time when running on Erlang.\n"
    " Runs in linear time with the length of the string otherwise.\n"
).
-spec prepend_string(bytes_builder(), binary()) -> bytes_builder().
prepend_string(Second, First) ->
    gleam_stdlib:iodata_append(gleam_stdlib:wrap_list(First), Second).

-file("src/gleam/bytes_builder.gleam", 99).
?DOC(
    " Appends a string onto the end of a builder.\n"
    "\n"
    " Runs in constant time when running on Erlang.\n"
    " Runs in linear time with the length of the string otherwise.\n"
).
-spec append_string(bytes_builder(), binary()) -> bytes_builder().
append_string(First, Second) ->
    gleam_stdlib:iodata_append(First, gleam_stdlib:wrap_list(Second)).

-file("src/gleam/bytes_builder.gleam", 142).
?DOC(
    " Creates a new builder from a string builder.\n"
    "\n"
    " Runs in constant time when running on Erlang.\n"
    " Runs in linear time otherwise.\n"
).
-spec from_string_builder(gleam@string_builder:string_builder()) -> bytes_builder().
from_string_builder(Builder) ->
    gleam_stdlib:wrap_list(Builder).

-file("src/gleam/bytes_builder.gleam", 151).
?DOC(
    " Creates a new builder from a bit array.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec from_bit_array(bitstring()) -> bytes_builder().
from_bit_array(Bits) ->
    gleam_stdlib:wrap_list(Bits).

-file("src/gleam/bytes_builder.gleam", 44).
?DOC(
    " Prepends a bit array to the start of a builder.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec prepend(bytes_builder(), bitstring()) -> bytes_builder().
prepend(Second, First) ->
    gleam_stdlib:iodata_append(gleam_stdlib:wrap_list(First), Second).

-file("src/gleam/bytes_builder.gleam", 52).
?DOC(
    " Appends a bit array to the end of a builder.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec append(bytes_builder(), bitstring()) -> bytes_builder().
append(First, Second) ->
    gleam_stdlib:iodata_append(First, gleam_stdlib:wrap_list(Second)).

-file("src/gleam/bytes_builder.gleam", 120).
?DOC(
    " Joins a list of bit arrays into a single builder.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec concat_bit_arrays(list(bitstring())) -> bytes_builder().
concat_bit_arrays(Bits) ->
    gleam_stdlib:identity(Bits).

-file("src/gleam/bytes_builder.gleam", 170).
-spec to_list(list(list(bytes_builder())), list(bitstring())) -> list(bitstring()).
to_list(Stack, Acc) ->
    case Stack of
        [] ->
            Acc;

        [[] | Remaining_stack] ->
            to_list(Remaining_stack, Acc);

        [[{bytes, Bits} | Rest] | Remaining_stack@1] ->
            to_list([Rest | Remaining_stack@1], [Bits | Acc]);

        [[{text, Builder} | Rest@1] | Remaining_stack@2] ->
            Bits@1 = gleam_stdlib:identity(
                gleam@string_builder:to_string(Builder)
            ),
            to_list([Rest@1 | Remaining_stack@2], [Bits@1 | Acc]);

        [[{many, Builders} | Rest@2] | Remaining_stack@3] ->
            to_list([Builders, Rest@2 | Remaining_stack@3], Acc)
    end.

-file("src/gleam/bytes_builder.gleam", 163).
?DOC(
    " Turns an builder into a bit array.\n"
    "\n"
    " Runs in linear time.\n"
    "\n"
    " When running on Erlang this function is implemented natively by the\n"
    " virtual machine and is highly optimised.\n"
).
-spec to_bit_array(bytes_builder()) -> bitstring().
to_bit_array(Builder) ->
    erlang:list_to_bitstring(Builder).

-file("src/gleam/bytes_builder.gleam", 197).
?DOC(
    " Returns the size of the builder's content in bytes.\n"
    "\n"
    " Runs in linear time.\n"
).
-spec byte_size(bytes_builder()) -> integer().
byte_size(Builder) ->
    erlang:iolist_size(Builder).
