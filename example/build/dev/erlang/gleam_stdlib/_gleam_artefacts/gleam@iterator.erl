-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/iterator.gleam").
-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type action(BWJ) :: stop | {continue, BWJ, fun(() -> action(BWJ))}.

-opaque iterator(BWK) :: {iterator, fun(() -> action(BWK))}.

-type step(BWL, BWM) :: {next, BWL, BWM} | done.

-type chunk(BWN, BWO) :: {another_by,
        list(BWN),
        BWO,
        BWN,
        fun(() -> action(BWN))} |
    {last_by, list(BWN)}.

-type sized_chunk(BWP) :: {another, list(BWP), fun(() -> action(BWP))} |
    {last, list(BWP)} |
    no_more.

-file("src/gleam/iterator.gleam", 38).
-spec stop() -> action(any()).
stop() ->
    stop.

-file("src/gleam/iterator.gleam", 43).
-spec do_unfold(BWS, fun((BWS) -> step(BWT, BWS))) -> fun(() -> action(BWT)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-file("src/gleam/iterator.gleam", 76).
?DOC(
    " Creates an iterator from a given function and accumulator.\n"
    "\n"
    " The function is called on the accumulator and returns either `Done`,\n"
    " indicating the iterator has no more elements, or `Next` which contains a\n"
    " new element and accumulator. The element is yielded by the iterator and the\n"
    " new accumulator is used with the function to compute the next element in\n"
    " the sequence.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " unfold(from: 5, with: fn(n) {\n"
    "  case n {\n"
    "    0 -> Done\n"
    "    n -> Next(element: n, accumulator: n - 1)\n"
    "  }\n"
    " })\n"
    " |> to_list\n"
    " // -> [5, 4, 3, 2, 1]\n"
    " ```\n"
).
-spec unfold(BWX, fun((BWX) -> step(BWY, BWX))) -> iterator(BWY).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-file("src/gleam/iterator.gleam", 89).
?DOC(
    " Creates an iterator that yields values created by calling a given function\n"
    " repeatedly.\n"
).
-spec repeatedly(fun(() -> BXC)) -> iterator(BXC).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-file("src/gleam/iterator.gleam", 104).
?DOC(
    " Creates an iterator that returns the same value infinitely.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " repeat(10)\n"
    " |> take(4)\n"
    " |> to_list\n"
    " // -> [10, 10, 10, 10]\n"
    " ```\n"
).
-spec repeat(BXE) -> iterator(BXE).
repeat(X) ->
    repeatedly(fun() -> X end).

-file("src/gleam/iterator.gleam", 118).
?DOC(
    " Creates an iterator that yields each element from the given list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4])\n"
    " |> to_list\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec from_list(list(BXG)) -> iterator(BXG).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-file("src/gleam/iterator.gleam", 129).
-spec do_transform(
    fun(() -> action(BXJ)),
    BXL,
    fun((BXL, BXJ) -> step(BXM, BXL))
) -> fun(() -> action(BXM)).
do_transform(Continuation, State, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                case F(State, El) of
                    done ->
                        stop;

                    {next, Yield, Next_state} ->
                        {continue, Yield, do_transform(Next, Next_state, F)}
                end
        end end.

-file("src/gleam/iterator.gleam", 164).
?DOC(
    " Creates an iterator from an existing iterator\n"
    " and a stateful function that may short-circuit.\n"
    "\n"
    " `f` takes arguments `acc` for current state and `el` for current element from underlying iterator,\n"
    " and returns either `Next` with yielded element and new state value, or `Done` to halt the iterator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Approximate implementation of `index` in terms of `transform`:\n"
    "\n"
    " ```gleam\n"
    " from_list([\"a\", \"b\", \"c\"])\n"
    " |> transform(0, fn(i, el) { Next(#(i, el), i + 1) })\n"
    " |> to_list\n"
    " // -> [#(0, \"a\"), #(1, \"b\"), #(2, \"c\")]\n"
    " ```\n"
).
-spec transform(iterator(BXQ), BXS, fun((BXS, BXQ) -> step(BXT, BXS))) -> iterator(BXT).
transform(Iterator, Initial, F) ->
    _pipe = do_transform(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 173).
-spec do_fold(fun(() -> action(BXX)), fun((BXZ, BXX) -> BXZ), BXZ) -> BXZ.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-file("src/gleam/iterator.gleam", 201).
?DOC(
    " Reduces an iterator of elements into a single value by calling a given\n"
    " function on each element in turn.\n"
    "\n"
    " If called on an iterator of infinite length then this function will never\n"
    " return.\n"
    "\n"
    " If you do not care about the end value and only wish to evaluate the\n"
    " iterator for side effects consider using the `run` function instead.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4])\n"
    " |> fold(from: 0, with: fn(acc, element) { element + acc })\n"
    " // -> 10\n"
    " ```\n"
).
-spec fold(iterator(BYA), BYC, fun((BYC, BYA) -> BYC)) -> BYC.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-file("src/gleam/iterator.gleam", 215).
?DOC(
    " Evaluates all elements emitted by the given iterator. This function is useful for when\n"
    " you wish to trigger any side effects that would occur when evaluating\n"
    " the iterator.\n"
).
-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-file("src/gleam/iterator.gleam", 233).
?DOC(
    " Evaluates an iterator and returns all the elements as a list.\n"
    "\n"
    " If called on an iterator of infinite length then this function will never\n"
    " return.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3])\n"
    " |> map(fn(x) { x * 2 })\n"
    " |> to_list\n"
    " // -> [2, 4, 6]\n"
    " ```\n"
).
-spec to_list(iterator(BYF)) -> list(BYF).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-file("src/gleam/iterator.gleam", 261).
?DOC(
    " Eagerly accesses the first value of an iterator, returning a `Next`\n"
    " that contains the first value and the rest of the iterator.\n"
    "\n"
    " If called on an empty iterator, `Done` is returned.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Next(first, rest) = from_list([1, 2, 3, 4]) |> step\n"
    "\n"
    " first\n"
    " // -> 1\n"
    "\n"
    " rest |> to_list\n"
    " // -> [2, 3, 4]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " empty() |> step\n"
    " // -> Done\n"
    " ```\n"
).
-spec step(iterator(BYI)) -> step(BYI, iterator(BYI)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-file("src/gleam/iterator.gleam", 268).
-spec do_take(fun(() -> action(BYN)), integer()) -> fun(() -> action(BYN)).
do_take(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, do_take(Next, Desired - 1)}
                end
        end end.

-file("src/gleam/iterator.gleam", 301).
?DOC(
    " Creates an iterator that only yields the first `desired` elements.\n"
    "\n"
    " If the iterator does not have enough elements all of them are yielded.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 5])\n"
    " |> take(up_to: 3)\n"
    " |> to_list\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2])\n"
    " |> take(up_to: 3)\n"
    " |> to_list\n"
    " // -> [1, 2]\n"
    " ```\n"
).
-spec take(iterator(BYQ), integer()) -> iterator(BYQ).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-file("src/gleam/iterator.gleam", 307).
-spec do_drop(fun(() -> action(BYT)), integer()) -> action(BYT).
do_drop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    do_drop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-file("src/gleam/iterator.gleam", 343).
?DOC(
    " Evaluates and discards the first N elements in an iterator, returning a new\n"
    " iterator.\n"
    "\n"
    " If the iterator does not have enough elements an empty iterator is\n"
    " returned.\n"
    "\n"
    " This function does not evaluate the elements of the iterator, the\n"
    " computation is performed when the iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 5])\n"
    " |> drop(up_to: 3)\n"
    " |> to_list\n"
    " // -> [4, 5]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2])\n"
    " |> drop(up_to: 3)\n"
    " |> to_list\n"
    " // -> []\n"
    " ```\n"
).
-spec drop(iterator(BYW), integer()) -> iterator(BYW).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 348).
-spec do_map(fun(() -> action(BYZ)), fun((BYZ) -> BZB)) -> fun(() -> action(BZB)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-file("src/gleam/iterator.gleam", 374).
?DOC(
    " Creates an iterator from an existing iterator and a transformation function.\n"
    "\n"
    " Each element in the new iterator will be the result of calling the given\n"
    " function on the elements in the given iterator.\n"
    "\n"
    " This function does not evaluate the elements of the iterator, the\n"
    " computation is performed when the iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3])\n"
    " |> map(fn(x) { x * 2 })\n"
    " |> to_list\n"
    " // -> [2, 4, 6]\n"
    " ```\n"
).
-spec map(iterator(BZD), fun((BZD) -> BZF)) -> iterator(BZF).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-file("src/gleam/iterator.gleam", 380).
-spec do_map2(
    fun(() -> action(BZH)),
    fun(() -> action(BZJ)),
    fun((BZH, BZJ) -> BZL)
) -> fun(() -> action(BZL)).
do_map2(Continuation1, Continuation2, Fun) ->
    fun() -> case Continuation1() of
            stop ->
                stop;

            {continue, A, Next_a} ->
                case Continuation2() of
                    stop ->
                        stop;

                    {continue, B, Next_b} ->
                        {continue, Fun(A, B), do_map2(Next_a, Next_b, Fun)}
                end
        end end.

-file("src/gleam/iterator.gleam", 421).
?DOC(
    " Combines two iterators into a single one using the given function.\n"
    "\n"
    " If an iterator is longer than the other the extra elements are dropped.\n"
    "\n"
    " This function does not evaluate the elements of the two iterators, the\n"
    " computation is performed when the resulting iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let first = from_list([1, 2, 3])\n"
    " let second = from_list([4, 5, 6])\n"
    " map2(first, second, fn(x, y) { x + y }) |> to_list\n"
    " // -> [5, 7, 9]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let first = from_list([1, 2])\n"
    " let second = from_list([\"a\", \"b\", \"c\"])\n"
    " map2(first, second, fn(i, x) { #(i, x) }) |> to_list\n"
    " // -> [#(1, \"a\"), #(2, \"b\")]\n"
    " ```\n"
).
-spec map2(iterator(BZN), iterator(BZP), fun((BZN, BZP) -> BZR)) -> iterator(BZR).
map2(Iterator1, Iterator2, Fun) ->
    _pipe = do_map2(
        erlang:element(2, Iterator1),
        erlang:element(2, Iterator2),
        Fun
    ),
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 430).
-spec do_append(fun(() -> action(BZT)), fun(() -> action(BZT))) -> action(BZT).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-file("src/gleam/iterator.gleam", 451).
?DOC(
    " Appends two iterators, producing a new iterator.\n"
    "\n"
    " This function does not evaluate the elements of the iterators, the\n"
    " computation is performed when the resulting iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2])\n"
    " |> append(from_list([3, 4]))\n"
    " |> to_list\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec append(iterator(BZX), iterator(BZX)) -> iterator(BZX).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 456).
-spec do_flatten(fun(() -> action(iterator(CAB)))) -> action(CAB).
do_flatten(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_iterator} ->
            do_append(
                erlang:element(2, It),
                fun() -> do_flatten(Next_iterator) end
            )
    end.

-file("src/gleam/iterator.gleam", 479).
?DOC(
    " Flattens an iterator of iterators, creating a new iterator.\n"
    "\n"
    " This function does not evaluate the elements of the iterator, the\n"
    " computation is performed when the iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([[1, 2], [3, 4]])\n"
    " |> map(from_list)\n"
    " |> flatten\n"
    " |> to_list\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec flatten(iterator(iterator(CAF))) -> iterator(CAF).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 499).
?DOC(
    " Joins a list of iterators into a single iterator.\n"
    "\n"
    " This function does not evaluate the elements of the iterator, the\n"
    " computation is performed when the iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [[1, 2], [3, 4]]\n"
    " |> map(from_list)\n"
    " |> concat\n"
    " |> to_list\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec concat(list(iterator(CAJ))) -> iterator(CAJ).
concat(Iterators) ->
    flatten(from_list(Iterators)).

-file("src/gleam/iterator.gleam", 521).
?DOC(
    " Creates an iterator from an existing iterator and a transformation function.\n"
    "\n"
    " Each element in the new iterator will be the result of calling the given\n"
    " function on the elements in the given iterator and then flattening the\n"
    " results.\n"
    "\n"
    " This function does not evaluate the elements of the iterator, the\n"
    " computation is performed when the iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2])\n"
    " |> flat_map(fn(x) { from_list([x, x + 1]) })\n"
    " |> to_list\n"
    " // -> [1, 2, 2, 3]\n"
    " ```\n"
).
-spec flat_map(iterator(CAN), fun((CAN) -> iterator(CAP))) -> iterator(CAP).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-file("src/gleam/iterator.gleam", 530).
-spec do_filter(fun(() -> action(CAS)), fun((CAS) -> boolean())) -> action(CAS).
do_filter(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Iterator} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> do_filter(Iterator, Predicate) end};

                false ->
                    do_filter(Iterator, Predicate)
            end
    end.

-file("src/gleam/iterator.gleam", 563).
?DOC(
    " Creates an iterator from an existing iterator and a predicate function.\n"
    "\n"
    " The new iterator will contain elements from the first iterator for which\n"
    " the given function returns `True`.\n"
    "\n"
    " This function does not evaluate the elements of the iterator, the\n"
    " computation is performed when the iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/int\n"
    "\n"
    " from_list([1, 2, 3, 4])\n"
    " |> filter(int.is_even)\n"
    " |> to_list\n"
    " // -> [2, 4]\n"
    " ```\n"
).
-spec filter(iterator(CAV), fun((CAV) -> boolean())) -> iterator(CAV).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 571).
-spec do_filter_map(
    fun(() -> action(CAY)),
    fun((CAY) -> {ok, CBA} | {error, any()})
) -> action(CBA).
do_filter_map(Continuation, F) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {continue, E@1, fun() -> do_filter_map(Next, F) end};

                {error, _} ->
                    do_filter_map(Next, F)
            end
    end.

-file("src/gleam/iterator.gleam", 607).
?DOC(
    " Creates an iterator from an existing iterator and a transforming predicate function.\n"
    "\n"
    " The new iterator will contain elements from the first iterator for which\n"
    " the given function returns `Ok`, transformed to the value inside the `Ok`.\n"
    "\n"
    " This function does not evaluate the elements of the iterator, the\n"
    " computation is performed when the iterator is later run.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/string\n"
    " import gleam/int\n"
    "\n"
    " \"a1b2c3d4e5f\"\n"
    " |> string.to_graphemes\n"
    " |> from_list\n"
    " |> filter_map(int.parse)\n"
    " |> to_list\n"
    " // -> [1, 2, 3, 4, 5]\n"
    " ```\n"
).
-spec filter_map(iterator(CBF), fun((CBF) -> {ok, CBH} | {error, any()})) -> iterator(CBH).
filter_map(Iterator, F) ->
    _pipe = fun() -> do_filter_map(erlang:element(2, Iterator), F) end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 627).
?DOC(
    " Creates an iterator that repeats a given iterator infinitely.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2])\n"
    " |> cycle\n"
    " |> take(6)\n"
    " |> to_list\n"
    " // -> [1, 2, 1, 2, 1, 2]\n"
    " ```\n"
).
-spec cycle(iterator(CBM)) -> iterator(CBM).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-file("src/gleam/iterator.gleam", 673).
-spec do_find(fun(() -> action(CBQ)), fun((CBQ) -> boolean())) -> {ok, CBQ} |
    {error, nil}.
do_find(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    do_find(Next, F)
            end
    end.

-file("src/gleam/iterator.gleam", 707).
?DOC(
    " Finds the first element in a given iterator for which the given function returns\n"
    " `True`.\n"
    "\n"
    " Returns `Error(Nil)` if the function does not return `True` for any of the\n"
    " elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " find(from_list([1, 2, 3]), fn(x) { x > 2 })\n"
    " // -> Ok(3)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find(from_list([1, 2, 3]), fn(x) { x > 4 })\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find(empty(), fn(_) { True })\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec find(iterator(CBU), fun((CBU) -> boolean())) -> {ok, CBU} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-file("src/gleam/iterator.gleam", 715).
-spec do_find_map(
    fun(() -> action(CBY)),
    fun((CBY) -> {ok, CCA} | {error, any()})
) -> {ok, CCA} | {error, nil}.
do_find_map(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {ok, E@1};

                {error, _} ->
                    do_find_map(Next, F)
            end
    end.

-file("src/gleam/iterator.gleam", 752).
?DOC(
    " Finds the first element in a given iterator\n"
    " for which the given function returns `Ok(new_value)`,\n"
    " then returns the wrapped `new_value`.\n"
    "\n"
    " Returns `Error(Nil)` if no such element is found.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " find_map(from_list([\"a\", \"1\", \"2\"]), int.parse)\n"
    " // -> Ok(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find_map(from_list([\"a\", \"b\", \"c\"]), int.parse)\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find_map(from_list([]), int.parse)\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec find_map(iterator(CCG), fun((CCG) -> {ok, CCI} | {error, any()})) -> {ok,
        CCI} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find_map(_pipe, Is_desired).

-file("src/gleam/iterator.gleam", 760).
-spec do_index(fun(() -> action(CCO)), integer()) -> fun(() -> action({CCO,
    integer()})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, do_index(Continuation@1, Next + 1)}
        end end.

-file("src/gleam/iterator.gleam", 782).
?DOC(
    " Wraps values yielded from an iterator with indices, starting from 0.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([\"a\", \"b\", \"c\"]) |> index |> to_list\n"
    " // -> [#(\"a\", 0), #(\"b\", 1), #(\"c\", 2)]\n"
    " ```\n"
).
-spec index(iterator(CCR)) -> iterator({CCR, integer()}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-file("src/gleam/iterator.gleam", 797).
?DOC(
    " Creates an iterator that infinitely applies a function to a value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " iterate(1, fn(n) { n * 3 }) |> take(5) |> to_list\n"
    " // -> [1, 3, 9, 27, 81]\n"
    " ```\n"
).
-spec iterate(CCU, fun((CCU) -> CCU)) -> iterator(CCU).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-file("src/gleam/iterator.gleam", 804).
-spec do_take_while(fun(() -> action(CCW)), fun((CCW) -> boolean())) -> fun(() -> action(CCW)).
do_take_while(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, do_take_while(Next, Predicate)}
                end
        end end.

-file("src/gleam/iterator.gleam", 831).
?DOC(
    " Creates an iterator that yields elements while the predicate returns `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 2, 4])\n"
    " |> take_while(satisfying: fn(x) { x < 3 })\n"
    " |> to_list\n"
    " // -> [1, 2]\n"
    " ```\n"
).
-spec take_while(iterator(CCZ), fun((CCZ) -> boolean())) -> iterator(CCZ).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-file("src/gleam/iterator.gleam", 840).
-spec do_drop_while(fun(() -> action(CDC)), fun((CDC) -> boolean())) -> action(CDC).
do_drop_while(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    do_drop_while(Next, Predicate)
            end
    end.

-file("src/gleam/iterator.gleam", 866).
?DOC(
    " Creates an iterator that drops elements while the predicate returns `True`,\n"
    " and then yields the remaining elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 2, 5])\n"
    " |> drop_while(satisfying: fn(x) { x < 4 })\n"
    " |> to_list\n"
    " // -> [4, 2, 5]\n"
    " ```\n"
).
-spec drop_while(iterator(CDF), fun((CDF) -> boolean())) -> iterator(CDF).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 874).
-spec do_scan(fun(() -> action(CDI)), fun((CDK, CDI) -> CDK), CDK) -> fun(() -> action(CDK)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-file("src/gleam/iterator.gleam", 904).
?DOC(
    " Creates an iterator from an existing iterator and a stateful function.\n"
    "\n"
    " Specifically, this behaves like `fold`, but yields intermediate results.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " // Generate a sequence of partial sums\n"
    " from_list([1, 2, 3, 4, 5])\n"
    " |> scan(from: 0, with: fn(acc, el) { acc + el })\n"
    " |> to_list\n"
    " // -> [1, 3, 6, 10, 15]\n"
    " ```\n"
).
-spec scan(iterator(CDM), CDO, fun((CDO, CDM) -> CDO)) -> iterator(CDO).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-file("src/gleam/iterator.gleam", 914).
-spec do_zip(fun(() -> action(CDQ)), fun(() -> action(CDS))) -> fun(() -> action({CDQ,
    CDS})).
do_zip(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                            {El_left, El_right},
                            do_zip(Next_left, Next_right)}
                end
        end end.

-file("src/gleam/iterator.gleam", 943).
?DOC(
    " Zips two iterators together, emitting values from both\n"
    " until the shorter one runs out.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([\"a\", \"b\", \"c\"])\n"
    " |> zip(range(20, 30))\n"
    " |> to_list\n"
    " // -> [#(\"a\", 20), #(\"b\", 21), #(\"c\", 22)]\n"
    " ```\n"
).
-spec zip(iterator(CDV), iterator(CDX)) -> iterator({CDV, CDX}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 954).
-spec next_chunk(fun(() -> action(CEA)), fun((CEA) -> CEC), CEC, list(CEA)) -> chunk(CEA, CEC).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, lists:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by, lists:reverse(Current_chunk), Key, E, Next}
            end
    end.

-file("src/gleam/iterator.gleam", 972).
-spec do_chunk(fun(() -> action(CEG)), fun((CEG) -> CEI), CEI, CEG) -> action(list(CEG)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-file("src/gleam/iterator.gleam", 997).
?DOC(
    " Creates an iterator that emits chunks of elements\n"
    " for which `f` returns the same value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 2, 3, 4, 4, 6, 7, 7])\n"
    " |> chunk(by: fn(n) { n % 2 })\n"
    " |> to_list\n"
    " // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]\n"
    " ```\n"
).
-spec chunk(iterator(CEL), fun((CEL) -> any())) -> iterator(list(CEL)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 1017).
-spec next_sized_chunk(fun(() -> action(CEQ)), integer(), list(CEQ)) -> sized_chunk(CEQ).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, lists:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, lists:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-file("src/gleam/iterator.gleam", 1038).
-spec do_sized_chunk(fun(() -> action(CEU)), integer()) -> fun(() -> action(list(CEU))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-file("src/gleam/iterator.gleam", 1075).
?DOC(
    " Creates an iterator that emits chunks of given size.\n"
    "\n"
    " If the last chunk does not have `count` elements, it is yielded\n"
    " as a partial chunk, with less than `count` elements.\n"
    "\n"
    " For any `count` less than 1 this function behaves as if it was set to 1.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 5, 6])\n"
    " |> sized_chunk(into: 2)\n"
    " |> to_list\n"
    " // -> [[1, 2], [3, 4], [5, 6]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 5, 6, 7, 8])\n"
    " |> sized_chunk(into: 3)\n"
    " |> to_list\n"
    " // -> [[1, 2, 3], [4, 5, 6], [7, 8]]\n"
    " ```\n"
).
-spec sized_chunk(iterator(CEY), integer()) -> iterator(list(CEY)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-file("src/gleam/iterator.gleam", 1084).
-spec do_intersperse(fun(() -> action(CFC)), CFC) -> action(CFC).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-file("src/gleam/iterator.gleam", 1123).
?DOC(
    " Creates an iterator that yields the given `elem` element\n"
    " between elements emitted by the underlying iterator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " empty()\n"
    " |> intersperse(with: 0)\n"
    " |> to_list\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1])\n"
    " |> intersperse(with: 0)\n"
    " |> to_list\n"
    " // -> [1]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 5])\n"
    " |> intersperse(with: 0)\n"
    " |> to_list\n"
    " // -> [1, 0, 2, 0, 3, 0, 4, 0, 5]\n"
    " ```\n"
).
-spec intersperse(iterator(CFF), CFF) -> iterator(CFF).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 1136).
-spec do_any(fun(() -> action(CFI)), fun((CFI) -> boolean())) -> boolean().
do_any(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    true;

                false ->
                    do_any(Next, Predicate)
            end
    end.

-file("src/gleam/iterator.gleam", 1177).
?DOC(
    " Returns `True` if any element emitted by the iterator satisfies the given predicate,\n"
    " `False` otherwise.\n"
    "\n"
    " This function short-circuits once it finds a satisfying element.\n"
    "\n"
    " An empty iterator results in `False`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " empty()\n"
    " |> any(fn(n) { n % 2 == 0 })\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 5, 7, 9])\n"
    " |> any(fn(n) { n % 2 == 0 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 3, 5, 7, 9])\n"
    " |> any(fn(n) { n % 2 == 0 })\n"
    " // -> False\n"
    " ```\n"
).
-spec any(iterator(CFK), fun((CFK) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-file("src/gleam/iterator.gleam", 1185).
-spec do_all(fun(() -> action(CFM)), fun((CFM) -> boolean())) -> boolean().
do_all(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    do_all(Next, Predicate);

                false ->
                    false
            end
    end.

-file("src/gleam/iterator.gleam", 1226).
?DOC(
    " Returns `True` if all elements emitted by the iterator satisfy the given predicate,\n"
    " `False` otherwise.\n"
    "\n"
    " This function short-circuits once it finds a non-satisfying element.\n"
    "\n"
    " An empty iterator results in `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " empty()\n"
    " |> all(fn(n) { n % 2 == 0 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([2, 4, 6, 8])\n"
    " |> all(fn(n) { n % 2 == 0 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([2, 4, 5, 8])\n"
    " |> all(fn(n) { n % 2 == 0 })\n"
    " // -> False\n"
    " ```\n"
).
-spec all(iterator(CFO), fun((CFO) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-file("src/gleam/iterator.gleam", 1234).
-spec update_group_with(CFQ) -> fun((gleam@option:option(list(CFQ))) -> list(CFQ)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-file("src/gleam/iterator.gleam", 1243).
-spec group_updater(fun((CFU) -> CFV)) -> fun((gleam@dict:dict(CFV, list(CFU)), CFU) -> gleam@dict:dict(CFV, list(CFU))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-file("src/gleam/iterator.gleam", 1265).
?DOC(
    " Returns a `Dict(k, List(element))` of elements from the given iterator\n"
    " grouped with the given key function.\n"
    "\n"
    " The order within each group is preserved from the iterator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 5, 6])\n"
    " |> group(by: fn(n) { n % 3 })\n"
    " // -> dict.from_list([#(0, [3, 6]), #(1, [1, 4]), #(2, [2, 5])])\n"
    " ```\n"
).
-spec group(iterator(CGC), fun((CGC) -> CGE)) -> gleam@dict:dict(CGE, list(CGC)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@dict:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-file("src/gleam/iterator.gleam", 1295).
?DOC(
    " This function acts similar to fold, but does not take an initial state.\n"
    " Instead, it starts from the first yielded element\n"
    " and combines it with each subsequent element in turn using the given function.\n"
    " The function is called as `f(accumulator, current_element)`.\n"
    "\n"
    " Returns `Ok` to indicate a successful run, and `Error` if called on an empty iterator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([])\n"
    " |> reduce(fn(acc, x) { acc + x })\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4, 5])\n"
    " |> reduce(fn(acc, x) { acc + x })\n"
    " // -> Ok(15)\n"
    " ```\n"
).
-spec reduce(iterator(CGI), fun((CGI, CGI) -> CGI)) -> {ok, CGI} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-file("src/gleam/iterator.gleam", 1325).
?DOC(
    " Returns the last element in the given iterator.\n"
    "\n"
    " Returns `Error(Nil)` if the iterator is empty.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " empty() |> last\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " range(1, 10) |> last\n"
    " // -> Ok(10)\n"
    " ```\n"
).
-spec last(iterator(CGM)) -> {ok, CGM} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("src/gleam/iterator.gleam", 1339).
?DOC(
    " Creates an iterator that yields no elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " empty() |> to_list\n"
    " // -> []\n"
    " ```\n"
).
-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-file("src/gleam/iterator.gleam", 1352).
?DOC(
    " Creates an iterator that yields exactly one element provided by calling the given function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " once(fn() { 1 }) |> to_list\n"
    " // -> [1]\n"
    " ```\n"
).
-spec once(fun(() -> CGS)) -> iterator(CGS).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 652).
?DOC(
    " Creates an iterator of ints, starting at a given start int and stepping by\n"
    " one to a given end int.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " range(from: 1, to: 5) |> to_list\n"
    " // -> [1, 2, 3, 4, 5]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " range(from: 1, to: -2) |> to_list\n"
    " // -> [1, 0, -1, -2]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " range(from: 0, to: 0) |> to_list\n"
    " // -> [0]\n"
    " ```\n"
).
-spec range(integer(), integer()) -> iterator(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-file("src/gleam/iterator.gleam", 1366).
?DOC(
    " Creates an iterator that yields the given element exactly once.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " single(1) |> to_list\n"
    " // -> [1]\n"
    " ```\n"
).
-spec single(CGU) -> iterator(CGU).
single(Elem) ->
    once(fun() -> Elem end).

-file("src/gleam/iterator.gleam", 1370).
-spec do_interleave(fun(() -> action(CGW)), fun(() -> action(CGW))) -> action(CGW).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-file("src/gleam/iterator.gleam", 1400).
?DOC(
    " Creates an iterator that alternates between the two given iterators\n"
    " until both have run out.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4])\n"
    " |> interleave(from_list([11, 12, 13, 14]))\n"
    " |> to_list\n"
    " // -> [1, 11, 2, 12, 3, 13, 4, 14]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4])\n"
    " |> interleave(from_list([100]))\n"
    " |> to_list\n"
    " // -> [1, 100, 2, 3, 4]\n"
    " ```\n"
).
-spec interleave(iterator(CHA), iterator(CHA)) -> iterator(CHA).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-file("src/gleam/iterator.gleam", 1408).
-spec do_fold_until(
    fun(() -> action(CHE)),
    fun((CHG, CHE) -> gleam@list:continue_or_stop(CHG)),
    CHG
) -> CHG.
do_fold_until(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    do_fold_until(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-file("src/gleam/iterator.gleam", 1447).
?DOC(
    " Like `fold`, `fold_until` reduces an iterator of elements into a single value by calling a given\n"
    " function on each element in turn, but uses `list.ContinueOrStop` to determine\n"
    " whether or not to keep iterating.\n"
    "\n"
    " If called on an iterator of infinite length then this function will only ever\n"
    " return if the function returns `list.Stop`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/list\n"
    "\n"
    " let f = fn(acc, e) {\n"
    "   case e {\n"
    "     _ if e < 4 -> list.Continue(e + acc)\n"
    "     _ -> list.Stop(acc)\n"
    "   }\n"
    " }\n"
    "\n"
    " from_list([1, 2, 3, 4])\n"
    " |> fold_until(from: acc, with: f)\n"
    " // -> 6\n"
    " ```\n"
).
-spec fold_until(
    iterator(CHI),
    CHK,
    fun((CHK, CHI) -> gleam@list:continue_or_stop(CHK))
) -> CHK.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-file("src/gleam/iterator.gleam", 1456).
-spec do_try_fold(
    fun(() -> action(CHM)),
    fun((CHO, CHM) -> {ok, CHO} | {error, CHP}),
    CHO
) -> {ok, CHO} | {error, CHP}.
do_try_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            gleam@result:'try'(
                F(Accumulator, Elem),
                fun(Accumulator@1) -> do_try_fold(Next, F, Accumulator@1) end
            )
    end.

-file("src/gleam/iterator.gleam", 1489).
?DOC(
    " A variant of fold that might fail.\n"
    "\n"
    " The folding function should return `Result(accumulator, error)`.\n"
    " If the returned value is `Ok(accumulator)` try_fold will try the next value in the iterator.\n"
    " If the returned value is `Error(error)` try_fold will stop and return that error.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4])\n"
    " |> try_fold(0, fn(acc, i) {\n"
    "   case i < 3 {\n"
    "     True -> Ok(acc + i)\n"
    "     False -> Error(Nil)\n"
    "   }\n"
    " })\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec try_fold(iterator(CHU), CHW, fun((CHW, CHU) -> {ok, CHW} | {error, CHX})) -> {ok,
        CHW} |
    {error, CHX}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-file("src/gleam/iterator.gleam", 1512).
?DOC(
    " Returns the first element yielded by the given iterator, if it exists,\n"
    " or `Error(Nil)` otherwise.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3]) |> first\n"
    " // -> Ok(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " empty() |> first\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec first(iterator(CIC)) -> {ok, CIC} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-file("src/gleam/iterator.gleam", 1542).
?DOC(
    " Returns nth element yielded by the given iterator, where `0` means the first element.\n"
    "\n"
    " If there are not enough elements in the iterator, `Error(Nil)` is returned.\n"
    "\n"
    " For any `index` less than `0` this function behaves as if it was set to `0`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4]) |> at(2)\n"
    " // -> Ok(3)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4]) |> at(4)\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " empty() |> at(0)\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec at(iterator(CIG), integer()) -> {ok, CIG} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-file("src/gleam/iterator.gleam", 1548).
-spec do_length(fun(() -> action(any())), integer()) -> integer().
do_length(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            do_length(Next, Length + 1)
    end.

-file("src/gleam/iterator.gleam", 1572).
?DOC(
    " Counts the number of elements in the given iterator.\n"
    "\n"
    " This function has to traverse the entire iterator to count its elements,\n"
    " so it runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " empty() |> length\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([1, 2, 3, 4]) |> length\n"
    " // -> 4\n"
    " ```\n"
).
-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    do_length(_pipe, 0).

-file("src/gleam/iterator.gleam", 1594).
?DOC(
    " Traverse an iterator, calling a function on each element.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " empty() |> each(io.println)\n"
    " // -> Nil\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([\"Tom\", \"Malory\", \"Louis\"]) |> each(io.println)\n"
    " // -> Nil\n"
    " // Tom\n"
    " // Malory\n"
    " // Louis\n"
    " ```\n"
).
-spec each(iterator(CIO), fun((CIO) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-file("src/gleam/iterator.gleam", 1619).
?DOC(
    " Add a new element to the start of an iterator.\n"
    "\n"
    " This function is for use with `use` expressions, to replicate the behaviour\n"
    " of the `yield` keyword found in other languages.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let iterator = {\n"
    "   use <- yield(1)\n"
    "   use <- yield(2)\n"
    "   use <- yield(3)\n"
    "   empty()\n"
    " }\n"
    "\n"
    " iterator |> to_list\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec yield(CIR, fun(() -> iterator(CIR))) -> iterator(CIR).
yield(Element, Next) ->
    {iterator,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.
