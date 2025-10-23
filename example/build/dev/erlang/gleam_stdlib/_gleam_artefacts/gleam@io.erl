-module(gleam@io).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/io.gleam").
-export([print/1, print_error/1, println/1, println_error/1, debug/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/gleam/io.gleam", 15).
?DOC(
    " Writes a string to standard output.\n"
    "\n"
    " If you want your output to be printed on its own line see `println`.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " io.print(\"Hi mum\")\n"
    " // -> Nil\n"
    " // Hi mum\n"
    " ```\n"
).
-spec print(binary()) -> nil.
print(String) ->
    gleam_stdlib:print(String).

-file("src/gleam/io.gleam", 35).
?DOC(
    " Writes a string to standard error.\n"
    "\n"
    " If you want your output to be printed on its own line see `println_error`.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```\n"
    " io.print_error(\"Hi pop\")\n"
    " // -> Nil\n"
    " // Hi pop\n"
    " ```\n"
).
-spec print_error(binary()) -> nil.
print_error(String) ->
    gleam_stdlib:print_error(String).

-file("src/gleam/io.gleam", 53).
?DOC(
    " Writes a string to standard output, appending a newline to the end.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " io.println(\"Hi mum\")\n"
    " // -> Nil\n"
    " // Hi mum\n"
    " ```\n"
).
-spec println(binary()) -> nil.
println(String) ->
    gleam_stdlib:println(String).

-file("src/gleam/io.gleam", 71).
?DOC(
    " Writes a string to standard error, appending a newline to the end.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " io.println_error(\"Hi pop\")\n"
    " // -> Nil\n"
    " // Hi pop\n"
    " ```\n"
).
-spec println_error(binary()) -> nil.
println_error(String) ->
    gleam_stdlib:println_error(String).

-file("src/gleam/io.gleam", 108).
?DOC(
    " Prints a value to standard error (stderr) yielding Gleam syntax.\n"
    "\n"
    " The value is returned after being printed so it can be used in pipelines.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " debug(\"Hi mum\")\n"
    " // -> \"Hi mum\"\n"
    " // <<\"Hi mum\">>\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " debug(Ok(1))\n"
    " // -> Ok(1)\n"
    " // {ok, 1}\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " import gleam/list\n"
    "\n"
    " [1, 2]\n"
    " |> list.map(fn(x) { x + 1 })\n"
    " |> debug\n"
    " |> list.map(fn(x) { x * 2 })\n"
    " // -> [4, 6]\n"
    " // [2, 3]\n"
    " ```\n"
).
-spec debug(EUG) -> EUG.
debug(Term) ->
    _pipe = Term,
    _pipe@1 = gleam@string:inspect(_pipe),
    gleam_stdlib:println_error(_pipe@1),
    Term.
