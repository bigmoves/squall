-module(gleam@string_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/string_builder.gleam").
-export([prepend_builder/2, append_builder/2, new/0, from_strings/1, concat/1, from_string/1, prepend/2, append/2, to_string/1, byte_size/1, join/2, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, is_equal/2, is_empty/1]).
-export_type([string_builder/0, direction/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type string_builder() :: any().

-type direction() :: all.

-file("src/gleam/string_builder.gleam", 51).
?DOC(
    " Prepends some `StringBuilder` onto the start of another.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec prepend_builder(string_builder(), string_builder()) -> string_builder().
prepend_builder(Builder, Prefix) ->
    gleam_stdlib:iodata_append(Prefix, Builder).

-file("src/gleam/string_builder.gleam", 62).
?DOC(
    " Appends some `StringBuilder` onto the end of another.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec append_builder(string_builder(), string_builder()) -> string_builder().
append_builder(Builder, Suffix) ->
    gleam_stdlib:iodata_append(Builder, Suffix).

-file("src/gleam/string_builder.gleam", 24).
?DOC(
    " Create an empty `StringBuilder`. Useful as the start of a pipe chaining many\n"
    " builders together.\n"
).
-spec new() -> string_builder().
new() ->
    gleam_stdlib:identity([]).

-file("src/gleam/string_builder.gleam", 77).
?DOC(
    " Converts a list of strings into a builder.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec from_strings(list(binary())) -> string_builder().
from_strings(Strings) ->
    gleam_stdlib:identity(Strings).

-file("src/gleam/string_builder.gleam", 89).
?DOC(
    " Joins a list of builders into a single builder.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec concat(list(string_builder())) -> string_builder().
concat(Builders) ->
    gleam_stdlib:identity(Builders).

-file("src/gleam/string_builder.gleam", 101).
?DOC(
    " Converts a string into a builder.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec from_string(binary()) -> string_builder().
from_string(String) ->
    gleam_stdlib:identity(String).

-file("src/gleam/string_builder.gleam", 32).
?DOC(
    " Prepends a `String` onto the start of some `StringBuilder`.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec prepend(string_builder(), binary()) -> string_builder().
prepend(Builder, Prefix) ->
    append_builder(from_string(Prefix), Builder).

-file("src/gleam/string_builder.gleam", 43).
?DOC(
    " Appends a `String` onto the end of some `StringBuilder`.\n"
    "\n"
    " Runs in constant time.\n"
).
-spec append(string_builder(), binary()) -> string_builder().
append(Builder, Second) ->
    append_builder(Builder, from_string(Second)).

-file("src/gleam/string_builder.gleam", 114).
?DOC(
    " Turns an `StringBuilder` into a `String`\n"
    "\n"
    " This function is implemented natively by the virtual machine and is highly\n"
    " optimised.\n"
).
-spec to_string(string_builder()) -> binary().
to_string(Builder) ->
    unicode:characters_to_binary(Builder).

-file("src/gleam/string_builder.gleam", 124).
?DOC(" Returns the size of the `StringBuilder` in bytes.\n").
-spec byte_size(string_builder()) -> integer().
byte_size(Builder) ->
    erlang:iolist_size(Builder).

-file("src/gleam/string_builder.gleam", 134).
?DOC(" Joins the given builders into a new builder separated with the given string\n").
-spec join(list(string_builder()), binary()) -> string_builder().
join(Builders, Sep) ->
    _pipe = Builders,
    _pipe@1 = gleam@list:intersperse(_pipe, from_string(Sep)),
    concat(_pipe@1).

-file("src/gleam/string_builder.gleam", 143).
?DOC(
    " Converts a builder to a new builder where the contents have been\n"
    " lowercased.\n"
).
-spec lowercase(string_builder()) -> string_builder().
lowercase(Builder) ->
    string:lowercase(Builder).

-file("src/gleam/string_builder.gleam", 154).
?DOC(
    " Converts a builder to a new builder where the contents have been\n"
    " uppercased.\n"
).
-spec uppercase(string_builder()) -> string_builder().
uppercase(Builder) ->
    string:uppercase(Builder).

-file("src/gleam/string_builder.gleam", 164).
?DOC(" Converts a builder to a new builder with the contents reversed.\n").
-spec reverse(string_builder()) -> string_builder().
reverse(Builder) ->
    string:reverse(Builder).

-file("src/gleam/string_builder.gleam", 191).
-spec do_split(string_builder(), binary()) -> list(string_builder()).
do_split(Iodata, Pattern) ->
    string:split(Iodata, Pattern, all).

-file("src/gleam/string_builder.gleam", 182).
?DOC(" Splits a builder on a given pattern into a list of builders.\n").
-spec split(string_builder(), binary()) -> list(string_builder()).
split(Iodata, Pattern) ->
    do_split(Iodata, Pattern).

-file("src/gleam/string_builder.gleam", 202).
?DOC(" Replaces all instances of a pattern with a given string substitute.\n").
-spec replace(string_builder(), binary(), binary()) -> string_builder().
replace(Builder, Pattern, Substitute) ->
    gleam_stdlib:string_replace(Builder, Pattern, Substitute).

-file("src/gleam/string_builder.gleam", 227).
?DOC(
    " Compares two builders to determine if they have the same textual content.\n"
    "\n"
    " Comparing two iodata using the `==` operator may return `False` even if they\n"
    " have the same content as they may have been build in different ways, so\n"
    " using this function is often preferred.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_strings([\"a\", \"b\"]) == from_string(\"ab\")\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_equal(from_strings([\"a\", \"b\"]), from_string(\"ab\"))\n"
    " // -> True\n"
    " ```\n"
).
-spec is_equal(string_builder(), string_builder()) -> boolean().
is_equal(A, B) ->
    string:equal(A, B).

-file("src/gleam/string_builder.gleam", 251).
?DOC(
    " Inspects a builder to determine if it is equivalent to an empty string.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_string(\"ok\") |> is_empty\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_string(\"\") |> is_empty\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_strings([]) |> is_empty\n"
    " // -> True\n"
    " ```\n"
).
-spec is_empty(string_builder()) -> boolean().
is_empty(Builder) ->
    string:is_empty(Builder).
