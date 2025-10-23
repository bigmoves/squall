-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/function.gleam").
-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/gleam/function.gleam", 2).
-spec compose(fun((ERJ) -> ERK), fun((ERK) -> ERL)) -> fun((ERJ) -> ERL).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("src/gleam/function.gleam", 40).
?DOC(
    " Takes a function with `2` arguments (an arity of `2`), and returns the\n"
    " curried equivalent.\n"
    "\n"
    " `fn(a, b) -> c` becomes `fn(a) -> fn(b) -> c`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " *Currying* creates a new function that is identical to the given function\n"
    " except that arguments must now be supplied one by one over several function\n"
    " calls. It thus is the process of taking a function with `n` arguments\n"
    " and producing a sequence of `n` single-argument functions. Given:\n"
    "\n"
    " ```gleam\n"
    " fn my_fun(i: Int, s: String) -> String { ... }\n"
    " ```\n"
    "\n"
    " …calling `curry2(my_fun)` would return the curried equivalent, like so:\n"
    "\n"
    " ```gleam\n"
    " curry2(my_fun)\n"
    " // fn(Int) -> fn(String) -> String\n"
    " ```\n"
    "\n"
    " Currying is useful when you want to partially apply a function with\n"
    " some arguments and then pass it somewhere else, for example:\n"
    "\n"
    " ```gleam\n"
    " import gleam/list\n"
    "\n"
    " let multiply = curry2(fn(x, y) { x * y })\n"
    " list.map([1, 2, 3], multiply(2))\n"
    " // -> [2, 4, 6]\n"
    " ```\n"
).
-spec curry2(fun((ERM, ERN) -> ERO)) -> fun((ERM) -> fun((ERN) -> ERO)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("src/gleam/function.gleam", 51).
?DOC(
    " Takes a function with `3` arguments (an arity of `3`), and returns the\n"
    " curried equivalent.\n"
    "\n"
    " `fn(a, b, c) -> d` becomes `fn(a) -> fn(b) -> fn(c) -> d`.\n"
    "\n"
    " See [`curry2`](#curry2) for a detailed explanation.\n"
).
-spec curry3(fun((ERQ, ERR, ERS) -> ERT)) -> fun((ERQ) -> fun((ERR) -> fun((ERS) -> ERT))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("src/gleam/function.gleam", 62).
?DOC(
    " Takes a function with `4` arguments (an arity of `4`), and returns the\n"
    " curried equivalent.\n"
    "\n"
    " `fn(a, b, c, d) -> e` becomes `fn(a) -> fn(b) -> fn(c) -> fn(d) -> e`.\n"
    "\n"
    " See [`curry2`](#curry2) for a detailed explanation.\n"
).
-spec curry4(fun((ERV, ERW, ERX, ERY) -> ERZ)) -> fun((ERV) -> fun((ERW) -> fun((ERX) -> fun((ERY) -> ERZ)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("src/gleam/function.gleam", 74).
?DOC(
    " Takes a function with `5` arguments (an arity of `5`), and returns the\n"
    " curried equivalent.\n"
    "\n"
    " `fn(a, b, c, d, e) -> f` becomes\n"
    " `fn(a) -> fn(b) -> fn(c) -> fn(d) -> fn(e) -> f`.\n"
    "\n"
    " See [`curry2`](#curry2) for a detailed explanation.\n"
).
-spec curry5(fun((ESB, ESC, ESD, ESE, ESF) -> ESG)) -> fun((ESB) -> fun((ESC) -> fun((ESD) -> fun((ESE) -> fun((ESF) -> ESG))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("src/gleam/function.gleam", 86).
?DOC(
    " Takes a function with `6` arguments (an arity of `6`), and returns the\n"
    " curried equivalent.\n"
    "\n"
    " `fn(a, b, c, d, e, f) -> g` becomes\n"
    " `fn(a) -> fn(b) -> fn(c) -> fn(d) -> fn(e) -> fn(f) -> g`.\n"
    "\n"
    " See [`curry2`](#curry2) for a detailed explanation.\n"
).
-spec curry6(fun((ESI, ESJ, ESK, ESL, ESM, ESN) -> ESO)) -> fun((ESI) -> fun((ESJ) -> fun((ESK) -> fun((ESL) -> fun((ESM) -> fun((ESN) -> ESO)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("src/gleam/function.gleam", 95).
?DOC(
    " Takes a function that takes two arguments and returns a new function that\n"
    " takes the same two arguments, but in reverse order.\n"
).
-spec flip(fun((ESQ, ESR) -> ESS)) -> fun((ESR, ESQ) -> ESS).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("src/gleam/function.gleam", 101).
?DOC(" Takes a single argument and always returns its input value.\n").
-spec identity(EST) -> EST.
identity(X) ->
    X.

-file("src/gleam/function.gleam", 106).
-spec constant(ESU) -> fun((any()) -> ESU).
constant(Value) ->
    fun(_) -> Value end.

-file("src/gleam/function.gleam", 115).
?DOC(
    " Takes an argument and a single function,\n"
    " calls that function with that argument\n"
    " and returns that argument instead of the function return value.\n"
    " Useful for running synchronous side effects in a pipeline.\n"
).
-spec tap(ESW, fun((ESW) -> any())) -> ESW.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("src/gleam/function.gleam", 136).
?DOC(
    " Takes a function with arity one and an argument,\n"
    " calls that function with the argument and returns the function return value.\n"
    "\n"
    " Useful for concisely calling functions returned as a part of a pipeline.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let doubler = fn() {\n"
    "   fn(x: Int) { x * 2 }\n"
    " }\n"
    "\n"
    " doubler() |> apply1(2)\n"
    " // -> 4\n"
    " ```\n"
).
-spec apply1(fun((ESY) -> ESZ), ESY) -> ESZ.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("src/gleam/function.gleam", 146).
?DOC(
    " Takes a function with arity two and two arguments,\n"
    " calls that function with the arguments\n"
    " and returns the function return value.\n"
    "\n"
    " See [`apply1`](#apply1) for more details.\n"
).
-spec apply2(fun((ETA, ETB) -> ETC), ETA, ETB) -> ETC.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("src/gleam/function.gleam", 156).
?DOC(
    " Takes a function with arity three and three arguments,\n"
    " calls that function with the arguments\n"
    " and returns the function return value.\n"
    "\n"
    " See [`apply1`](#apply1) for more details.\n"
).
-spec apply3(fun((ETD, ETE, ETF) -> ETG), ETD, ETE, ETF) -> ETG.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
