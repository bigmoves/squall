-module(gleam@regex).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/regex.gleam").
-export([compile/2, from_string/1, check/2, split/2, scan/2, replace/3]).
-export_type([regex/0, match/0, compile_error/0, options/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This module contains regular expression matching functions for strings.\n"
    " The matching algorithms of the library are based on the PCRE library, but not\n"
    " all of the PCRE library is interfaced and some parts of the library go beyond\n"
    " what PCRE offers. Currently PCRE version 8.40 (release date 2017-01-11) is used.\n"
).

-type regex() :: any().

-type match() :: {match, binary(), list(gleam@option:option(binary()))}.

-type compile_error() :: {compile_error, binary(), integer()}.

-type options() :: {options, boolean(), boolean()}.

-file("src/gleam/regex.gleam", 55).
?DOC(
    " Creates a `Regex` with some additional options.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let options = Options(case_insensitive: False, multi_line: True)\n"
    " let assert Ok(re) = compile(\"^[0-9]\", with: options)\n"
    " check(re, \"abc\\n123\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let options = Options(case_insensitive: True, multi_line: False)\n"
    " let assert Ok(re) = compile(\"[A-Z]\", with: options)\n"
    " check(re, \"abc123\")\n"
    " // -> True\n"
    " ```\n"
).
-spec compile(binary(), options()) -> {ok, regex()} | {error, compile_error()}.
compile(Pattern, Options) ->
    gleam_stdlib:compile_regex(Pattern, Options).

-file("src/gleam/regex.gleam", 89).
?DOC(
    " Creates a new `Regex`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\"[0-9]\")\n"
    " check(re, \"abc123\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " check(re, \"abcxyz\")\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_string(\"[0-9\")\n"
    " // -> Error(CompileError(\n"
    " //   error: \"missing terminating ] for character class\",\n"
    " //   byte_index: 4\n"
    " // ))\n"
    " ```\n"
).
-spec from_string(binary()) -> {ok, regex()} | {error, compile_error()}.
from_string(Pattern) ->
    compile(Pattern, {options, false, false}).

-file("src/gleam/regex.gleam", 108).
?DOC(
    " Returns a boolean indicating whether there was a match or not.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\"^f.o.?\")\n"
    " check(with: re, content: \"foo\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " check(with: re, content: \"boo\")\n"
    " // -> False\n"
    " ```\n"
).
-spec check(regex(), binary()) -> boolean().
check(Regex, Content) ->
    gleam_stdlib:regex_check(Regex, Content).

-file("src/gleam/regex.gleam", 126).
?DOC(
    " Splits a string.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\" *, *\")\n"
    " split(with: re, content: \"foo,32, 4, 9  ,0\")\n"
    " // -> [\"foo\", \"32\", \"4\", \"9\", \"0\"]\n"
    " ```\n"
).
-spec split(regex(), binary()) -> list(binary()).
split(Regex, String) ->
    gleam_stdlib:regex_split(Regex, String).

-file("src/gleam/regex.gleam", 186).
?DOC(
    " Collects all matches of the regular expression.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\"[oi]n a (\\\\w+)\")\n"
    " scan(with: re, content: \"I am on a boat in a lake.\")\n"
    " // -> [\n"
    " //   Match(content: \"on a boat\", submatches: [Some(\"boat\")]),\n"
    " //   Match(content: \"in a lake\", submatches: [Some(\"lake\")]),\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = regex.from_string(\"([+|\\\\-])?(\\\\d+)(\\\\w+)?\")\n"
    " scan(with: re, content: \"-36\")\n"
    " // -> [\n"
    " //   Match(content: \"-36\", submatches: [Some(\"-\"), Some(\"36\")])\n"
    " // ]\n"
    "\n"
    " scan(with: re, content: \"36\")\n"
    " // -> [\n"
    " //   Match(content: \"36\", submatches: [None, Some(\"36\")])\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) =\n"
    "   regex.from_string(\"var\\\\s*(\\\\w+)\\\\s*(int|string)?\\\\s*=\\\\s*(.*)\")\n"
    " scan(with: re, content: \"var age = 32\")\n"
    " // -> [\n"
    " //   Match(\n"
    " //     content: \"var age = 32\",\n"
    " //     submatches: [Some(\"age\"), None, Some(\"32\")],\n"
    " //   ),\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = regex.from_string(\"let (\\\\w+) = (\\\\w+)\")\n"
    " scan(with: re, content: \"let age = 32\")\n"
    " // -> [\n"
    " //   Match(\n"
    " //     content: \"let age = 32\",\n"
    " //     submatches: [Some(\"age\"), Some(\"32\")],\n"
    " //   ),\n"
    " // ]\n"
    "\n"
    " scan(with: re, content: \"const age = 32\")\n"
    " // -> []\n"
    " ```\n"
).
-spec scan(regex(), binary()) -> list(match()).
scan(Regex, String) ->
    gleam_stdlib:regex_scan(Regex, String).

-file("src/gleam/regex.gleam", 212).
?DOC(
    " Creates a new `String` by replacing all substrings that match the regular\n"
    " expression.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert OK(re) = regex.from_string(\"^https://\")\n"
    " replace(each: re, in: \"https://example.com\", with: \"www.\")\n"
    " // -> \"www.example.com\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert OK(re) = regex.from_string(\"[, +-]\")\n"
    " replace(each: re, in: \"a,b-c d+e\", with: \"/\")\n"
    " // -> \"a/b/c/d/e\"\n"
    " `\n"
).
-spec replace(regex(), binary(), binary()) -> binary().
replace(Pattern, String, Substitute) ->
    gleam_stdlib:regex_replace(Pattern, String, Substitute).
