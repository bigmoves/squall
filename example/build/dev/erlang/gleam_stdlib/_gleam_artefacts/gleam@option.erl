-module(gleam@option).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/option.gleam").
-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type option(EW) :: {some, EW} | none.

-file("src/gleam/option.gleam", 24).
-spec do_all(list(option(EX)), list(EX)) -> option(list(EX)).
do_all(List, Acc) ->
    case List of
        [] ->
            {some, Acc};

        [X | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case {Acc@1, Item} of
                    {{some, Values}, {some, Value}} ->
                        {some, [Value | Values]};

                    {_, _} ->
                        none
                end end,
            Accumulate(do_all(Rest, Acc), X)
    end.

-file("src/gleam/option.gleam", 55).
?DOC(
    " Combines a list of `Option`s into a single `Option`.\n"
    " If all elements in the list are `Some` then returns a `Some` holding the list of values.\n"
    " If any element is `None` then returns`None`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " all([Some(1), Some(2)])\n"
    " // -> Some([1, 2])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " all([Some(1), None])\n"
    " // -> None\n"
    " ```\n"
).
-spec all(list(option(FD))) -> option(list(FD)).
all(List) ->
    do_all(List, []).

-file("src/gleam/option.gleam", 73).
?DOC(
    " Checks whether the `Option` is a `Some` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_some(Some(1))\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_some(None)\n"
    " // -> False\n"
    " ```\n"
).
-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-file("src/gleam/option.gleam", 91).
?DOC(
    " Checks whether the `Option` is a `None` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_none(Some(1))\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_none(None)\n"
    " // -> True\n"
    " ```\n"
).
-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-file("src/gleam/option.gleam", 109).
?DOC(
    " Converts an `Option` type to a `Result` type.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " to_result(Some(1), \"some_error\")\n"
    " // -> Ok(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " to_result(None, \"some_error\")\n"
    " // -> Error(\"some_error\")\n"
    " ```\n"
).
-spec to_result(option(FM), FP) -> {ok, FM} | {error, FP}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _ ->
            {error, E}
    end.

-file("src/gleam/option.gleam", 130).
?DOC(
    " Converts a `Result` type to an `Option` type.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_result(Ok(1))\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_result(Error(\"some_error\"))\n"
    " // -> None\n"
    " ```\n"
).
-spec from_result({ok, FS} | {error, any()}) -> option(FS).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _ ->
            none
    end.

-file("src/gleam/option.gleam", 151).
?DOC(
    " Extracts the value from an `Option`, returning a default value if there is none.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " unwrap(Some(1), 0)\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " unwrap(None, 0)\n"
    " // -> 0\n"
    " ```\n"
).
-spec unwrap(option(FX), FX) -> FX.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-file("src/gleam/option.gleam", 172).
?DOC(
    " Extracts the value from an `Option`, evaluating the default function if the option is `None`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " lazy_unwrap(Some(1), fn() { 0 })\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_unwrap(None, fn() { 0 })\n"
    " // -> 0\n"
    " ```\n"
).
-spec lazy_unwrap(option(FZ), fun(() -> FZ)) -> FZ.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-file("src/gleam/option.gleam", 197).
?DOC(
    " Updates a value held within the `Some` of an `Option` by calling a given function\n"
    " on it.\n"
    "\n"
    " If the `Option` is a `None` rather than `Some`, the function is not called and the\n"
    " `Option` stays the same.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map(over: Some(1), with: fn(x) { x + 1 })\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " map(over: None, with: fn(x) { x + 1 })\n"
    " // -> None\n"
    " ```\n"
).
-spec map(option(GB), fun((GB) -> GD)) -> option(GD).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-file("src/gleam/option.gleam", 223).
?DOC(
    " Merges a nested `Option` into a single layer.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " flatten(Some(Some(1)))\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " flatten(Some(None))\n"
    " // -> None\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " flatten(None)\n"
    " // -> None\n"
    " ```\n"
).
-spec flatten(option(option(GF))) -> option(GF).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-file("src/gleam/option.gleam", 262).
?DOC(
    " Updates a value held within the `Some` of an `Option` by calling a given function\n"
    " on it, where the given function also returns an `Option`. The two options are\n"
    " then merged together into one `Option`.\n"
    "\n"
    " If the `Option` is a `None` rather than `Some` the function is not called and the\n"
    " option stays the same.\n"
    "\n"
    " This function is the equivalent of calling `map` followed by `flatten`, and\n"
    " it is useful for chaining together multiple functions that return `Option`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " then(Some(1), fn(x) { Some(x + 1) })\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " then(Some(1), fn(x) { Some(#(\"a\", x)) })\n"
    " // -> Some(#(\"a\", 1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " then(Some(1), fn(_) { None })\n"
    " // -> None\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " then(None, fn(x) { Some(x + 1) })\n"
    " // -> None\n"
    " ```\n"
).
-spec then(option(GJ), fun((GJ) -> option(GL))) -> option(GL).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-file("src/gleam/option.gleam", 293).
?DOC(
    " Returns the first value if it is `Some`, otherwise returns the second value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " or(Some(1), Some(2))\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " or(Some(1), None)\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " or(None, Some(2))\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " or(None, None)\n"
    " // -> None\n"
    " ```\n"
).
-spec 'or'(option(GO), option(GO)) -> option(GO).
'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.

-file("src/gleam/option.gleam", 324).
?DOC(
    " Returns the first value if it is `Some`, otherwise evaluates the given function for a fallback value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " lazy_or(Some(1), fn() { Some(2) })\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_or(Some(1), fn() { None })\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_or(None, fn() { Some(2) })\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_or(None, fn() { None })\n"
    " // -> None\n"
    " ```\n"
).
-spec lazy_or(option(GS), fun(() -> option(GS))) -> option(GS).
lazy_or(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second()
    end.

-file("src/gleam/option.gleam", 331).
-spec do_values(list(option(GW)), list(GW)) -> list(GW).
do_values(List, Acc) ->
    case List of
        [] ->
            Acc;

        [X | Xs] ->
            Accumulate = fun(Acc@1, Item) -> case Item of
                    {some, Value} ->
                        [Value | Acc@1];

                    none ->
                        Acc@1
                end end,
            Accumulate(do_values(Xs, Acc), X)
    end.

-file("src/gleam/option.gleam", 356).
?DOC(
    " Given a list of `Option`s,\n"
    " returns only the values inside `Some`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " values([Some(1), None, Some(3)])\n"
    " // -> [1, 3]\n"
    " ```\n"
).
-spec values(list(option(HB))) -> list(HB).
values(Options) ->
    do_values(Options, []).
