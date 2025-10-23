-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/list.gleam").
-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1, sorting/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Lists are an ordered sequence of elements and are one of the most common\n"
    " data types in Gleam.\n"
    "\n"
    " New elements can be added and removed from the front of a list in\n"
    " constant time, while adding and removing from the end requires traversing\n"
    " and copying the whole list, so keep this in mind when designing your\n"
    " programs.\n"
    "\n"
    " There is a dedicated syntax for prefixing to a list:\n"
    "\n"
    " ```gleam\n"
    " let new_list = [1, 2, ..existing_list]\n"
    " ```\n"
    "\n"
    " And a matching syntax for getting the first elements of a list:\n"
    "\n"
    " ```gleam\n"
    " case list {\n"
    "   [first_element, ..rest] -> first_element\n"
    "   _ -> \"this pattern matches when the list is empty\"\n"
    " }\n"
    " ```\n"
    "\n"
).

-type continue_or_stop(YS) :: {continue, YS} | {stop, YS}.

-type sorting() :: ascending | descending.

-file("src/gleam/list.gleam", 61).
-spec count_length(list(any()), integer()) -> integer().
count_length(List, Count) ->
    case List of
        [_ | List@1] ->
            count_length(List@1, Count + 1);

        _ ->
            Count
    end.

-file("src/gleam/list.gleam", 57).
?DOC(
    " Counts the number of elements in a given list.\n"
    "\n"
    " This function has to traverse the list to determine the number of elements,\n"
    " so it runs in linear time.\n"
    "\n"
    " This function is natively implemented by the virtual machine and is highly\n"
    " optimised.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " length([])\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length([1])\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length([1, 2])\n"
    " // -> 2\n"
    " ```\n"
).
-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-file("src/gleam/list.gleam", 130).
-spec do_reverse(list(ARP), list(ARP)) -> list(ARP).
do_reverse(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            do_reverse(Rest, [Item | Accumulator])
    end.

-file("src/gleam/list.gleam", 126).
?DOC(
    " Creates a new list from a given list containing the same elements but in the\n"
    " opposite order.\n"
    "\n"
    " This function has to traverse the list to create the new reversed list, so\n"
    " it runs in linear time.\n"
    "\n"
    " This function is natively implemented by the virtual machine and is highly\n"
    " optimised.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " reverse([])\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " reverse([1])\n"
    " // -> [1]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " reverse([1, 2])\n"
    " // -> [2, 1]\n"
    " ```\n"
).
-spec reverse(list(YZ)) -> list(YZ).
reverse(Xs) ->
    lists:reverse(Xs).

-file("src/gleam/list.gleam", 158).
?DOC(
    " Determines whether or not the list is empty.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_empty([])\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_empty([1])\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_empty([1, 1])\n"
    " // -> False\n"
    " ```\n"
).
-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-file("src/gleam/list.gleam", 194).
?DOC(
    " Determines whether or not a given element exists within a given list.\n"
    "\n"
    " This function traverses the list to find the element, so it runs in linear\n"
    " time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> contains(any: 0)\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [0] |> contains(any: 0)\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1] |> contains(any: 0)\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 1] |> contains(any: 0)\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 0] |> contains(any: 0)\n"
    " // -> True\n"
    " ```\n"
).
-spec contains(list(AAH), AAH) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-file("src/gleam/list.gleam", 221).
?DOC(
    " Gets the first element from the start of the list, if there is one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " first([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " first([0])\n"
    " // -> Ok(0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " first([1, 2])\n"
    " // -> Ok(1)\n"
    " ```\n"
).
-spec first(list(AAJ)) -> {ok, AAJ} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-file("src/gleam/list.gleam", 250).
?DOC(
    " Returns the list minus the first element. If the list is empty, `Error(Nil)` is\n"
    " returned.\n"
    "\n"
    " This function runs in constant time and does not make a copy of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " rest([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " rest([0])\n"
    " // -> Ok([])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " rest([1, 2])\n"
    " // -> Ok([2])\n"
    " ```\n"
).
-spec rest(list(AAN)) -> {ok, list(AAN)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

-file("src/gleam/list.gleam", 257).
-spec update_group(fun((AAS) -> AAT)) -> fun((gleam@dict:dict(AAT, list(AAS)), AAS) -> gleam@dict:dict(AAT, list(AAS))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-file("src/gleam/list.gleam", 304).
-spec do_filter(list(ABG), fun((ABG) -> boolean()), list(ABG)) -> list(ABG).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-file("src/gleam/list.gleam", 332).
?DOC(
    " Returns a new list containing only the elements from the first list for\n"
    " which the given functions returns `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " filter([2, 4, 6, 1], fn(x) { x > 2 })\n"
    " // -> [4, 6]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " filter([2, 4, 6, 1], fn(x) { x > 6 })\n"
    " // -> []\n"
    " ```\n"
).
-spec filter(list(ABK), fun((ABK) -> boolean())) -> list(ABK).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-file("src/gleam/list.gleam", 336).
-spec do_filter_map(
    list(ABN),
    fun((ABN) -> {ok, ABP} | {error, any()}),
    list(ABP)
) -> list(ABP).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-file("src/gleam/list.gleam", 368).
?DOC(
    " Returns a new list containing only the elements from the first list for\n"
    " which the given functions returns `Ok(_)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " filter_map([2, 4, 6, 1], Error)\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " filter_map([2, 4, 6, 1], fn(x) { Ok(x + 1) })\n"
    " // -> [3, 5, 7, 2]\n"
    " ```\n"
).
-spec filter_map(list(ABV), fun((ABV) -> {ok, ABX} | {error, any()})) -> list(ABX).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-file("src/gleam/list.gleam", 372).
-spec do_map(list(ACC), fun((ACC) -> ACE), list(ACE)) -> list(ACE).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-file("src/gleam/list.gleam", 389).
?DOC(
    " Returns a new list containing only the elements of the first list after the\n"
    " function has been applied to each one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map([2, 4, 6], fn(x) { x * 2 })\n"
    " // -> [4, 8, 12]\n"
    " ```\n"
).
-spec map(list(ACH), fun((ACH) -> ACJ)) -> list(ACJ).
map(List, Fun) ->
    do_map(List, Fun, []).

-file("src/gleam/list.gleam", 413).
-spec do_map2(list(ACR), list(ACT), fun((ACR, ACT) -> ACV), list(ACV)) -> list(ACV).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-file("src/gleam/list.gleam", 409).
?DOC(
    " Combines two lists into a single list using the given function.\n"
    "\n"
    " If a list is longer than the other the extra elements are dropped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map2([1, 2, 3], [4, 5, 6], fn(x, y) { x + y })\n"
    " // -> [5, 7, 9]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " map2([1, 2], [\"a\", \"b\", \"c\"], fn(i, x) { #(i, x) })\n"
    " // -> [#(1, \"a\"), #(2, \"b\")]\n"
    " ```\n"
).
-spec map2(list(ACL), list(ACN), fun((ACL, ACN) -> ACP)) -> list(ACP).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-file("src/gleam/list.gleam", 451).
-spec do_index_map(
    list(ADD),
    fun((ADD, integer()) -> ADF),
    integer(),
    list(ADF)
) -> list(ADF).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(X, Index) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-file("src/gleam/list.gleam", 479).
?DOC(
    " Returns a new list containing only the elements of the first list after the\n"
    " function has been applied to each one and their index.\n"
    "\n"
    " The index starts at 0, so the first element is 0, the second is 1, and so\n"
    " on.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " index_map([\"a\", \"b\"], fn(x, i) { #(i, x) })\n"
    " // -> [#(0, \"a\"), #(1, \"b\")]\n"
    " ```\n"
).
-spec index_map(list(ADI), fun((ADI, integer()) -> ADK)) -> list(ADK).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-file("src/gleam/list.gleam", 483).
-spec do_try_map(list(ADM), fun((ADM) -> {ok, ADO} | {error, ADP}), list(ADO)) -> {ok,
        list(ADO)} |
    {error, ADP}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("src/gleam/list.gleam", 530).
?DOC(
    " Takes a function that returns a `Result` and applies it to each element in a\n"
    " given list in turn.\n"
    "\n"
    " If the function returns `Ok(new_value)` for all elements in the list then a\n"
    " list of the new values is returned.\n"
    "\n"
    " If the function returns `Error(reason)` for any of the elements then it is\n"
    " returned immediately. None of the elements in the list are processed after\n"
    " one returns an `Error`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " try_map([1, 2, 3], fn(x) { Ok(x + 2) })\n"
    " // -> Ok([3, 4, 5])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " try_map([1, 2, 3], fn(_) { Error(0) })\n"
    " // -> Error(0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " try_map([[1], [2, 3]], first)\n"
    " // -> Ok([1, 2])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " try_map([[1], [], [2]], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec try_map(list(ADW), fun((ADW) -> {ok, ADY} | {error, ADZ})) -> {ok,
        list(ADY)} |
    {error, ADZ}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-file("src/gleam/list.gleam", 557).
?DOC(
    " Returns a list that is the given list with up to the given number of\n"
    " elements removed from the front of the list.\n"
    "\n"
    " If the element has less than the number of elements an empty list is\n"
    " returned.\n"
    "\n"
    " This function runs in linear time but does not copy the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " drop([1, 2, 3, 4], 2)\n"
    " // -> [3, 4]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " drop([1, 2, 3, 4], 9)\n"
    " // -> []\n"
    " ```\n"
).
-spec drop(list(AEF), integer()) -> list(AEF).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-file("src/gleam/list.gleam", 568).
-spec do_take(list(AEI), integer(), list(AEI)) -> list(AEI).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-file("src/gleam/list.gleam", 599).
?DOC(
    " Returns a list containing the first given number of elements from the given\n"
    " list.\n"
    "\n"
    " If the element has less than the number of elements then the full list is\n"
    " returned.\n"
    "\n"
    " This function runs in linear time but does not copy the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " take([1, 2, 3, 4], 2)\n"
    " // -> [1, 2]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " take([1, 2, 3, 4], 9)\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec take(list(AEM), integer()) -> list(AEM).
take(List, N) ->
    do_take(List, N, []).

-file("src/gleam/list.gleam", 612).
?DOC(
    " Returns a new empty list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " // -> []\n"
    " ```\n"
).
-spec new() -> list(any()).
new() ->
    [].

-file("src/gleam/list.gleam", 632).
?DOC(
    " Returns the given item wrapped in a list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " wrap(1)\n"
    " // -> [1]\n"
    "\n"
    " wrap([\"a\", \"b\", \"c\"])\n"
    " // -> [[\"a\", \"b\", \"c\"]]\n"
    "\n"
    " wrap([[]])\n"
    " // -> [[[]]]\n"
    " ```\n"
).
-spec wrap(AER) -> list(AER).
wrap(Item) ->
    [Item].

-file("src/gleam/list.gleam", 653).
-spec do_append(list(AEX), list(AEX)) -> list(AEX).
do_append(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            do_append(Rest, [Item | Second])
    end.

-file("src/gleam/list.gleam", 649).
?DOC(
    " Joins one list onto the end of another.\n"
    "\n"
    " This function runs in linear time, and it traverses and copies the first\n"
    " list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " append([1, 2], [3])\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec append(list(AET), list(AET)) -> list(AET).
append(First, Second) ->
    lists:append(First, Second).

-file("src/gleam/list.gleam", 673).
?DOC(
    " Prefixes an item to a list. This can also be done using the dedicated\n"
    " syntax instead\n"
    "\n"
    " ```gleam\n"
    " let existing_list = [2, 3, 4]\n"
    "\n"
    " [1, ..existing_list]\n"
    " // -> [1, 2, 3, 4]\n"
    "\n"
    " prepend(to: existing_list, this: 1)\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec prepend(list(AFB), AFB) -> list(AFB).
prepend(List, Item) ->
    [Item | List].

-file("src/gleam/list.gleam", 678).
-spec reverse_and_prepend(list(AFE), list(AFE)) -> list(AFE).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-file("src/gleam/list.gleam", 685).
-spec do_concat(list(list(AFI)), list(AFI)) -> list(AFI).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-file("src/gleam/list.gleam", 704).
?DOC(
    " Joins a list of lists into a single list.\n"
    "\n"
    " This function traverses all elements twice.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " concat([[1], [2, 3], []])\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec concat(list(list(AFN))) -> list(AFN).
concat(Lists) ->
    do_concat(Lists, []).

-file("src/gleam/list.gleam", 720).
?DOC(
    " This is the same as `concat`: it joins a list of lists into a single\n"
    " list.\n"
    "\n"
    " This function traverses all elements twice.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " flatten([[1], [2, 3], []])\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec flatten(list(list(AFR))) -> list(AFR).
flatten(Lists) ->
    do_concat(Lists, []).

-file("src/gleam/list.gleam", 733).
?DOC(
    " Maps the list with the given function into a list of lists, and then flattens it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " flat_map([2, 4, 6], fn(x) { [x, x + 1] })\n"
    " // -> [2, 3, 4, 5, 6, 7]\n"
    " ```\n"
).
-spec flat_map(list(AFV), fun((AFV) -> list(AFX))) -> list(AFX).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    concat(_pipe).

-file("src/gleam/list.gleam", 746).
?DOC(
    " Reduces a list of elements into a single value by calling a given function\n"
    " on each element, going from left to right.\n"
    "\n"
    " `fold([1, 2, 3], 0, add)` is the equivalent of\n"
    " `add(add(add(0, 1), 2), 3)`.\n"
    "\n"
    " This function runs in linear time.\n"
).
-spec fold(list(AGA), AGC, fun((AGC, AGA) -> AGC)) -> AGC.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-file("src/gleam/list.gleam", 90).
?DOC(
    " Counts the number of elements in a given list satisfying a given predicate.\n"
    "\n"
    " This function has to traverse the list to determine the number of elements,\n"
    " so it runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " count([], fn(a) { a > 0 })\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " count([1], fn(a) { a > 0 })\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " count([1, 2, 3], int.is_odd)\n"
    " // -> 2\n"
    " ```\n"
).
-spec count(list(YX), fun((YX) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-file("src/gleam/list.gleam", 300).
?DOC(
    " Takes a list and groups the values by a key\n"
    " which is built from a key function.\n"
    "\n"
    " Does not preserve the initial value order.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    "\n"
    " [Ok(3), Error(\"Wrong\"), Ok(200), Ok(73)]\n"
    " |> group(by: fn(i) {\n"
    "   case i {\n"
    "     Ok(_) -> \"Successful\"\n"
    "     Error(_) -> \"Failed\"\n"
    "   }\n"
    " })\n"
    " |> dict.to_list\n"
    " // -> [\n"
    " //   #(\"Failed\", [Error(\"Wrong\")]),\n"
    " //   #(\"Successful\", [Ok(73), Ok(200), Ok(3)])\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    "\n"
    " group([1,2,3,4,5], by: fn(i) { i - i / 3 * 3 })\n"
    " |> dict.to_list\n"
    " // -> [#(0, [3]), #(1, [4, 1]), #(2, [5, 2])]\n"
    " ```\n"
).
-spec group(list(ABA), fun((ABA) -> ABC)) -> gleam@dict:dict(ABC, list(ABA)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-file("src/gleam/list.gleam", 438).
?DOC(
    " Similar to `map` but also lets you pass around an accumulated value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map_fold(\n"
    "   over: [1, 2, 3],\n"
    "   from: 100,\n"
    "   with: fn(memo, i) { #(memo + i, i * 2) }\n"
    " )\n"
    " // -> #(106, [2, 4, 6])\n"
    " ```\n"
).
-spec map_fold(list(ACY), ADA, fun((ADA, ACY) -> {ADA, ADB})) -> {ADA,
    list(ADB)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-file("src/gleam/list.gleam", 768).
?DOC(
    " Reduces a list of elements into a single value by calling a given function\n"
    " on each element, going from right to left.\n"
    "\n"
    " `fold_right([1, 2, 3], 0, add)` is the equivalent of\n"
    " `add(add(add(0, 3), 2), 1)`.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " Unlike `fold` this function is not tail recursive. Where possible use\n"
    " `fold` instead as it will use less memory.\n"
).
-spec fold_right(list(AGD), AGF, fun((AGF, AGD) -> AGF)) -> AGF.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-file("src/gleam/list.gleam", 779).
-spec do_index_fold(
    list(AGG),
    AGI,
    fun((AGI, AGG, integer()) -> AGI),
    integer()
) -> AGI.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-file("src/gleam/list.gleam", 801).
?DOC(
    " Like fold but the folding function also receives the index of the current element.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\"a\", \"b\", \"c\"]\n"
    " |> index_fold([], fn(acc, item, index) { ... })\n"
    " ```\n"
).
-spec index_fold(list(AGJ), AGL, fun((AGL, AGJ, integer()) -> AGL)) -> AGL.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-file("src/gleam/list.gleam", 828).
?DOC(
    " A variant of fold that might fail.\n"
    "\n"
    " The folding function should return `Result(accumulator, error)`.\n"
    " If the returned value is `Ok(accumulator)` try_fold will try the next value in the list.\n"
    " If the returned value is `Error(error)` try_fold will stop and return that error.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4]\n"
    " |> try_fold(0, fn(acc, i) {\n"
    "   case i < 3 {\n"
    "     True -> Ok(acc + i)\n"
    "     False -> Error(Nil)\n"
    "   }\n"
    " })\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec try_fold(list(AGM), AGO, fun((AGO, AGM) -> {ok, AGO} | {error, AGP})) -> {ok,
        AGO} |
    {error, AGP}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-file("src/gleam/list.gleam", 867).
?DOC(
    " A variant of fold that allows to stop folding earlier.\n"
    "\n"
    " The folding function should return `ContinueOrStop(accumulator)`.\n"
    " If the returned value is `Continue(accumulator)` fold_until will try the next value in the list.\n"
    " If the returned value is `Stop(accumulator)` fold_until will stop and return that accumulator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4]\n"
    " |> fold_until(0, fn(acc, i) {\n"
    "   case i < 3 {\n"
    "     True -> Continue(acc + i)\n"
    "     False -> Stop(acc)\n"
    "   }\n"
    " })\n"
    " // -> 6\n"
    " ```\n"
).
-spec fold_until(list(AGU), AGW, fun((AGW, AGU) -> continue_or_stop(AGW))) -> AGW.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-file("src/gleam/list.gleam", 904).
?DOC(
    " Finds the first element in a given list for which the given function returns\n"
    " `True`.\n"
    "\n"
    " Returns `Error(Nil)` if no such element is found.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " find([1, 2, 3], fn(x) { x > 2 })\n"
    " // -> Ok(3)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find([1, 2, 3], fn(x) { x > 4 })\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find([], fn(_) { True })\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec find(list(AGY), fun((AGY) -> boolean())) -> {ok, AGY} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-file("src/gleam/list.gleam", 940).
?DOC(
    " Finds the first element in a given list for which the given function returns\n"
    " `Ok(new_value)`, then returns the wrapped `new_value`.\n"
    "\n"
    " Returns `Error(Nil)` if no such element is found.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " find_map([[], [2], [3]], first)\n"
    " // -> Ok(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find_map([[], []], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find_map([], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec find_map(list(AHC), fun((AHC) -> {ok, AHE} | {error, any()})) -> {ok, AHE} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-file("src/gleam/list.gleam", 975).
?DOC(
    " Returns `True` if the given function returns `True` for all the elements in\n"
    " the given list. If the function returns `False` for any of the elements it\n"
    " immediately returns `False` without checking the rest of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " all([], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " all([4, 5], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " all([4, 3], fn(x) { x > 3 })\n"
    " // -> False\n"
    " ```\n"
).
-spec all(list(AHK), fun((AHK) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-file("src/gleam/list.gleam", 1012).
?DOC(
    " Returns `True` if the given function returns `True` for any the elements in\n"
    " the given list. If the function returns `True` for any of the elements it\n"
    " immediately returns `True` without checking the rest of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " any([], fn(x) { x > 3 })\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " any([4, 5], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " any([4, 3], fn(x) { x > 4 })\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " any([3, 4], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
).
-spec any(list(AHM), fun((AHM) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-file("src/gleam/list.gleam", 1023).
-spec do_zip(list(AHO), list(AHQ), list({AHO, AHQ})) -> list({AHO, AHQ}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-file("src/gleam/list.gleam", 1057).
?DOC(
    " Takes two lists and returns a single list of 2-element tuples.\n"
    "\n"
    " If one of the lists is longer than the other, the remaining elements from\n"
    " the longer list are not used.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " zip([], [])\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " zip([1, 2], [3])\n"
    " // -> [#(1, 3)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " zip([1], [3, 4])\n"
    " // -> [#(1, 3)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " zip([1, 2], [3, 4])\n"
    " // -> [#(1, 3), #(2, 4)]\n"
    " ```\n"
).
-spec zip(list(AHU), list(AHW)) -> list({AHU, AHW}).
zip(List, Other) ->
    do_zip(List, Other, []).

-file("src/gleam/list.gleam", 1087).
?DOC(
    " Takes two lists and returns a single list of 2-element tuples.\n"
    "\n"
    " If one of the lists is longer than the other, an `Error` is returned.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " strict_zip([], [])\n"
    " // -> Ok([])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " strict_zip([1, 2], [3])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " strict_zip([1], [3, 4])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " strict_zip([1, 2], [3, 4])\n"
    " // -> Ok([#(1, 3), #(2, 4)])\n"
    " ```\n"
).
-spec strict_zip(list(AHZ), list(AIB)) -> {ok, list({AHZ, AIB})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-file("src/gleam/list.gleam", 1097).
-spec do_unzip(list({AYU, AYV}), list(AYU), list(AYV)) -> {list(AYU), list(AYV)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {lists:reverse(Xs), lists:reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-file("src/gleam/list.gleam", 1118).
?DOC(
    " Takes a single list of 2-element tuples and returns two lists.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " unzip([#(1, 2), #(3, 4)])\n"
    " // -> #([1, 3], [2, 4])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " unzip([])\n"
    " // -> #([], [])\n"
    " ```\n"
).
-spec unzip(list({AIK, AIL})) -> {list(AIK), list(AIL)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-file("src/gleam/list.gleam", 1122).
-spec do_intersperse(list(AIP), AIP, list(AIP)) -> list(AIP).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-file("src/gleam/list.gleam", 1145).
?DOC(
    " Inserts a given value between each existing element in a given list.\n"
    "\n"
    " This function runs in linear time and copies the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " intersperse([1, 1, 1], 2)\n"
    " // -> [1, 2, 1, 2, 1]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " intersperse([], 2)\n"
    " // -> []\n"
    " ```\n"
).
-spec intersperse(list(AIT), AIT) -> list(AIT).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-file("src/gleam/list.gleam", 1163).
?DOC(
    " Removes any duplicate elements from a given list.\n"
    "\n"
    " This function returns in loglinear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " unique([1, 1, 1, 4, 7, 3, 3, 4])\n"
    " // -> [1, 4, 7, 3]\n"
    " ```\n"
).
-spec unique(list(AIW)) -> list(AIW).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-file("src/gleam/list.gleam", 1244).
?DOC(
    " Given a list it returns slices of it that are locally sorted in ascending\n"
    " order.\n"
    "\n"
    " Imagine you have this list:\n"
    "\n"
    " ```\n"
    "   [1, 2, 3, 2, 1, 0]\n"
    "    ^^^^^^^  ^^^^^^^ This is a slice in descending order\n"
    "    |\n"
    "    | This is a slice that is sorted in ascending order\n"
    " ```\n"
    "\n"
    " So the produced result will contain these two slices, each one sorted in\n"
    " ascending order: `[[1, 2, 3], [0, 1, 2]]`.\n"
    "\n"
    " - `growing` is an accumulator with the current slice being grown\n"
    " - `direction` is the growing direction of the slice being grown, it could\n"
    "   either be ascending or strictly descending\n"
    " - `prev` is the previous element that needs to be added to the growing slice\n"
    "   it is carried around to check wether we have to keep growing the current\n"
    "   slice or not\n"
    " - `acc` is the accumulator containing the slices sorted in ascending order\n"
).
-spec sequences(
    list(AJC),
    fun((AJC, AJC) -> gleam@order:order()),
    list(AJC),
    sorting(),
    AJC,
    list(list(AJC))
) -> list(list(AJC)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [do_reverse(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-file("src/gleam/list.gleam", 1392).
?DOC(
    " Merges two lists sorted in ascending order into a single list sorted in\n"
    " descending order according to the given comparator function.\n"
    "\n"
    " This reversing of the sort order is not avoidable if we want to implement\n"
    " merge as a tail recursive function. We could reverse the accumulator before\n"
    " returning it but that would end up being less efficient; so the merging\n"
    " algorithm has to play around this.\n"
).
-spec merge_ascendings(
    list(AJZ),
    list(AJZ),
    fun((AJZ, AJZ) -> gleam@order:order()),
    list(AJZ)
) -> list(AJZ).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-file("src/gleam/list.gleam", 1345).
?DOC(
    " Given a list of ascending lists, it merges adjacent pairs into a single\n"
    " descending list, halving their number.\n"
    " It returns a list of the remaining descending lists.\n"
).
-spec merge_ascending_pairs(
    list(list(AJN)),
    fun((AJN, AJN) -> gleam@order:order()),
    list(list(AJN))
) -> list(list(AJN)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-file("src/gleam/list.gleam", 1419).
?DOC(
    " This is exactly the same as merge_ascendings but mirrored: it merges two\n"
    " lists sorted in descending order into a single list sorted in ascending\n"
    " order according to the given comparator function.\n"
    "\n"
    " This reversing of the sort order is not avoidable if we want to implement\n"
    " merge as a tail recursive function. We could reverse the accumulator before\n"
    " returning it but that would end up being less efficient; so the merging\n"
    " algorithm has to play around this.\n"
).
-spec merge_descendings(
    list(AKE),
    list(AKE),
    fun((AKE, AKE) -> gleam@order:order()),
    list(AKE)
) -> list(AKE).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-file("src/gleam/list.gleam", 1367).
?DOC(" This is the same as merge_ascending_pairs but flipped for descending lists.\n").
-spec merge_descending_pairs(
    list(list(AJT)),
    fun((AJT, AJT) -> gleam@order:order()),
    list(list(AJT))
) -> list(list(AJT)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-file("src/gleam/list.gleam", 1311).
?DOC(
    " Given some some sorted sequences (assumed to be sorted in `direction`) it\n"
    " merges them all together until we're left with just a list sorted in\n"
    " ascending order.\n"
).
-spec merge_all(
    list(list(AJJ)),
    sorting(),
    fun((AJJ, AJJ) -> gleam@order:order())
) -> list(AJJ).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            do_reverse(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-file("src/gleam/list.gleam", 1182).
?DOC(
    " Sorts from smallest to largest based upon the ordering specified by a given\n"
    " function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/int\n"
    "\n"
    " sort([4, 3, 6, 5, 4, 1, 2], by: int.compare)\n"
    " // -> [1, 2, 3, 4, 4, 5, 6]\n"
    " ```\n"
).
-spec sort(list(AIZ), fun((AIZ, AIZ) -> gleam@order:order())) -> list(AIZ).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-file("src/gleam/list.gleam", 1459).
-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            tail_recursive_range(Start, Stop + 1, [Stop | Acc]);

        lt ->
            tail_recursive_range(Start, Stop - 1, [Stop | Acc])
    end.

-file("src/gleam/list.gleam", 1455).
?DOC(
    " Creates a list of ints ranging from a given start and finish.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " range(0, 0)\n"
    " // -> [0]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " range(0, 5)\n"
    " // -> [0, 1, 2, 3, 4, 5]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " range(1, -5)\n"
    " // -> [1, 0, -1, -2, -3, -4, -5]\n"
    " ```\n"
).
-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-file("src/gleam/list.gleam", 1467).
-spec do_repeat(AKM, integer(), list(AKM)) -> list(AKM).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-file("src/gleam/list.gleam", 1488).
?DOC(
    " Builds a list of a given value a given number of times.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " repeat(\"a\", times: 0)\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " repeat(\"a\", times: 5)\n"
    " // -> [\"a\", \"a\", \"a\", \"a\", \"a\"]\n"
    " ```\n"
).
-spec repeat(AKP, integer()) -> list(AKP).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-file("src/gleam/list.gleam", 1492).
-spec do_split(list(AKR), integer(), list(AKR)) -> {list(AKR), list(AKR)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-file("src/gleam/list.gleam", 1525).
?DOC(
    " Splits a list in two before the given index.\n"
    "\n"
    " If the list is not long enough to have the given index the before list will\n"
    " be the input list, and the after list will be empty.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split([6, 7, 8, 9], 0)\n"
    " // -> #([], [6, 7, 8, 9])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " split([6, 7, 8, 9], 2)\n"
    " // -> #([6, 7], [8, 9])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " split([6, 7, 8, 9], 4)\n"
    " // -> #([6, 7, 8, 9], [])\n"
    " ```\n"
).
-spec split(list(AKW), integer()) -> {list(AKW), list(AKW)}.
split(List, Index) ->
    do_split(List, Index, []).

-file("src/gleam/list.gleam", 1529).
-spec do_split_while(list(ALA), fun((ALA) -> boolean()), list(ALA)) -> {list(ALA),
    list(ALA)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-file("src/gleam/list.gleam", 1562).
?DOC(
    " Splits a list in two before the first element that a given function returns\n"
    " `False` for.\n"
    "\n"
    " If the function returns `True` for all elements the first list will be the\n"
    " input list, and the second list will be empty.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })\n"
    " // -> #([1, 2, 3], [4, 5])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })\n"
    " // -> #([1, 2, 3, 4, 5], [])\n"
    " ```\n"
).
-spec split_while(list(ALF), fun((ALF) -> boolean())) -> {list(ALF), list(ALF)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-file("src/gleam/list.gleam", 1594).
?DOC(
    " Given a list of 2-element tuples, finds the first tuple that has a given\n"
    " key as the first element and returns the second element.\n"
    "\n"
    " If no tuple is found with the given key then `Error(Nil)` is returned.\n"
    "\n"
    " This function may be useful for interacting with Erlang code where lists of\n"
    " tuples are common.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_find([#(\"a\", 0), #(\"b\", 1)], \"a\")\n"
    " // -> Ok(0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_find([#(\"a\", 0), #(\"b\", 1)], \"b\")\n"
    " // -> Ok(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_find([#(\"a\", 0), #(\"b\", 1)], \"c\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec key_find(list({ALJ, ALK}), ALJ) -> {ok, ALK} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/list.gleam", 1625).
?DOC(
    " Given a list of 2-element tuples, finds all tuples that have a given\n"
    " key as the first element and returns the second element.\n"
    "\n"
    " This function may be useful for interacting with Erlang code where lists of\n"
    " tuples are common.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_filter([#(\"a\", 0), #(\"b\", 1), #(\"a\", 2)], \"a\")\n"
    " // -> [0, 2]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_filter([#(\"a\", 0), #(\"b\", 1)], \"c\")\n"
    " // -> []\n"
    " ```\n"
).
-spec key_filter(list({ALO, ALP}), ALO) -> list(ALP).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/list.gleam", 1638).
-spec do_pop(list(BDV), fun((BDV) -> boolean()), list(BDV)) -> {ok,
        {BDV, list(BDV)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-file("src/gleam/list.gleam", 1670).
?DOC(
    " Removes the first element in a given list for which the predicate function returns `True`.\n"
    "\n"
    " Returns `Error(Nil)` if no such element is found.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " pop([1, 2, 3], fn(x) { x > 2 })\n"
    " // -> Ok(#(3, [1, 2]))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pop([1, 2, 3], fn(x) { x > 4 })\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pop([], fn(_) { True })\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec pop(list(ALW), fun((ALW) -> boolean())) -> {ok, {ALW, list(ALW)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-file("src/gleam/list.gleam", 1677).
-spec do_pop_map(list(BEJ), fun((BEJ) -> {ok, BEW} | {error, any()}), list(BEJ)) -> {ok,
        {BEW, list(BEJ)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-file("src/gleam/list.gleam", 1710).
?DOC(
    " Removes the first element in a given list for which the given function returns\n"
    " `Ok(new_value)`, then returns the wrapped `new_value` as well as list with the value removed.\n"
    "\n"
    " Returns `Error(Nil)` if no such element is found.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " pop_map([[], [2], [3]], first)\n"
    " // -> Ok(#(2, [[], [3]]))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pop_map([[], []], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pop_map([], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec pop_map(list(AMF), fun((AMF) -> {ok, AMH} | {error, any()})) -> {ok,
        {AMH, list(AMF)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-file("src/gleam/list.gleam", 1740).
?DOC(
    " Given a list of 2-element tuples, finds the first tuple that has a given\n"
    " key as the first element. This function will return the second element\n"
    " of the found tuple and list with tuple removed.\n"
    "\n"
    " If no tuple is found with the given key then `Error(Nil)` is returned.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_pop([#(\"a\", 0), #(\"b\", 1)], \"a\")\n"
    " // -> Ok(#(0, [#(\"b\", 1)]))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_pop([#(\"a\", 0), #(\"b\", 1)], \"b\")\n"
    " // -> Ok(#(1, [#(\"a\", 0)]))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_pop([#(\"a\", 0), #(\"b\", 1)], \"c\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec key_pop(list({AMO, AMP}), AMO) -> {ok, {AMP, list({AMO, AMP})}} |
    {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/list.gleam", 1770).
?DOC(
    " Given a list of 2-element tuples, inserts a key and value into the list.\n"
    "\n"
    " If there was already a tuple with the key then it is replaced, otherwise it\n"
    " is added to the end of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_set([#(5, 0), #(4, 1)], 4, 100)\n"
    " // -> [#(5, 0), #(4, 100)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_set([#(5, 0), #(4, 1)], 1, 100)\n"
    " // -> [#(5, 0), #(4, 1), #(1, 100)]\n"
    " ```\n"
).
-spec key_set(list({AMU, AMV}), AMU, AMV) -> list({AMU, AMV}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-file("src/gleam/list.gleam", 1792).
?DOC(
    " Calls a function for each element in a list, discarding the return value.\n"
    "\n"
    " Useful for calling a side effect for every item of a list.\n"
    "\n"
    " ```gleam\n"
    " import gleam/io\n"
    "\n"
    " each([\"1\", \"2\", \"3\"], io.println)\n"
    " // -> Nil\n"
    " // 1\n"
    " // 2\n"
    " // 3\n"
    " ```\n"
).
-spec each(list(AMY), fun((AMY) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-file("src/gleam/list.gleam", 1818).
?DOC(
    " Calls a `Result` returning function for each element in a list, discarding\n"
    " the return value. If the function returns `Error` then the iteration is\n"
    " stopped and the error is returned.\n"
    "\n"
    " Useful for calling a side effect for every item of a list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " try_each(\n"
    "   over: [1, 2, 3],\n"
    "   with: function_that_might_fail,\n"
    " )\n"
    " // -> Ok(Nil)\n"
    " ```\n"
).
-spec try_each(list(ANB), fun((ANB) -> {ok, any()} | {error, ANE})) -> {ok, nil} |
    {error, ANE}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [X | Xs] ->
            case Fun(X) of
                {ok, _} ->
                    try_each(Xs, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-file("src/gleam/list.gleam", 1832).
-spec do_partition(list(BGD), fun((BGD) -> boolean()), list(BGD), list(BGD)) -> {list(BGD),
    list(BGD)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-file("src/gleam/list.gleam", 1855).
?DOC(
    " Partitions a list into a tuple/pair of lists\n"
    " by a given categorisation function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/int\n"
    "\n"
    " [1, 2, 3, 4, 5] |> partition(int.is_odd)\n"
    " // -> #([1, 3, 5], [2, 4])\n"
    " ```\n"
).
-spec partition(list(ANO), fun((ANO) -> boolean())) -> {list(ANO), list(ANO)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-file("src/gleam/list.gleam", 1871).
?DOC(
    " Returns all the permutations of a list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " permutations([1, 2])\n"
    " // -> [[1, 2], [2, 1]]\n"
    " ```\n"
).
-spec permutations(list(ANS)) -> list(list(ANS)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            _pipe = L,
            _pipe@5 = index_map(_pipe, fun(I, I_idx) -> _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = lists:reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end) end),
            concat(_pipe@5)
    end.

-file("src/gleam/list.gleam", 1892).
-spec do_window(list(list(ANW)), list(ANW), integer()) -> list(list(ANW)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case erlang:length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-file("src/gleam/list.gleam", 1915).
?DOC(
    " Returns a list of sliding windows.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " window([1,2,3,4,5], 3)\n"
    " // -> [[1, 2, 3], [2, 3, 4], [3, 4, 5]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " window([1, 2], 4)\n"
    " // -> []\n"
    " ```\n"
).
-spec window(list(AOC), integer()) -> list(list(AOC)).
window(L, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            _pipe = do_window([], L, N),
            lists:reverse(_pipe)
    end.

-file("src/gleam/list.gleam", 1936).
?DOC(
    " Returns a list of tuples containing two contiguous elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " window_by_2([1,2,3,4])\n"
    " // -> [#(1, 2), #(2, 3), #(3, 4)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " window_by_2([1])\n"
    " // -> []\n"
    " ```\n"
).
-spec window_by_2(list(AOG)) -> list({AOG, AOG}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-file("src/gleam/list.gleam", 1949).
?DOC(
    " Drops the first elements in a given list for which the predicate function returns `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " drop_while([1, 2, 3, 4], fn (x) { x < 3 })\n"
    " // -> [3, 4]\n"
    " ```\n"
).
-spec drop_while(list(AOJ), fun((AOJ) -> boolean())) -> list(AOJ).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-file("src/gleam/list.gleam", 1963).
-spec do_take_while(list(AOM), fun((AOM) -> boolean()), list(AOM)) -> list(AOM).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-file("src/gleam/list.gleam", 1987).
?DOC(
    " Takes the first elements in a given list for which the predicate function returns `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " take_while([1, 2, 3, 2, 4], fn (x) { x < 3 })\n"
    " // -> [1, 2]\n"
    " ```\n"
).
-spec take_while(list(AOQ), fun((AOQ) -> boolean())) -> list(AOQ).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-file("src/gleam/list.gleam", 1994).
-spec do_chunk(list(AOT), fun((AOT) -> AOV), AOV, list(AOT), list(list(AOT))) -> list(list(AOT)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-file("src/gleam/list.gleam", 2026).
?DOC(
    " Returns a list of chunks in which\n"
    " the return value of calling `f` on each element is the same.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 2, 3, 4, 4, 6, 7, 7] |> chunk(by: fn(n) { n % 2 })\n"
    " // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]\n"
    " ```\n"
).
-spec chunk(list(APB), fun((APB) -> any())) -> list(list(APB)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-file("src/gleam/list.gleam", 2033).
-spec do_sized_chunk(
    list(APG),
    integer(),
    integer(),
    list(APG),
    list(list(APG))
) -> list(list(APG)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-file("src/gleam/list.gleam", 2075).
?DOC(
    " Returns a list of chunks containing `count` elements each.\n"
    "\n"
    " If the last chunk does not have `count` elements, it is instead\n"
    " a partial chunk, with less than `count` elements.\n"
    "\n"
    " For any `count` less than 1 this function behaves as if it was set to 1.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4, 5, 6] |> sized_chunk(into: 2)\n"
    " // -> [[1, 2], [3, 4], [5, 6]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4, 5, 6, 7, 8] |> sized_chunk(into: 3)\n"
    " // -> [[1, 2, 3], [4, 5, 6], [7, 8]]\n"
    " ```\n"
).
-spec sized_chunk(list(APN), integer()) -> list(list(APN)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-file("src/gleam/list.gleam", 2099).
?DOC(
    " This function acts similar to fold, but does not take an initial state.\n"
    " Instead, it starts from the first element in the list\n"
    " and combines it with each subsequent element in turn using the given\n"
    " function. The function is called as `fun(accumulator, current_element)`.\n"
    "\n"
    " Returns `Ok` to indicate a successful run, and `Error` if called on an\n"
    " empty list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> reduce(fn(acc, x) { acc + x })\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4, 5] |> reduce(fn(acc, x) { acc + x })\n"
    " // -> Ok(15)\n"
    " ```\n"
).
-spec reduce(list(APR), fun((APR, APR) -> APR)) -> {ok, APR} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-file("src/gleam/list.gleam", 2106).
-spec do_scan(list(APV), APX, list(APX), fun((APX, APV) -> APX)) -> list(APX).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-file("src/gleam/list.gleam", 2130).
?DOC(
    " Similar to `fold`, but yields the state of the accumulator at each stage.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " scan(over: [1, 2, 3], from: 100, with: fn(acc, i) { acc + i })\n"
    " // -> [101, 103, 106]\n"
    " ```\n"
).
-spec scan(list(AQA), AQC, fun((AQC, AQA) -> AQC)) -> list(AQC).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-file("src/gleam/list.gleam", 2158).
?DOC(
    " Returns the last element in the given list.\n"
    "\n"
    " Returns `Error(Nil)` if the list is empty.\n"
    "\n"
    " This function runs in linear time.\n"
    " For a collection oriented around performant access at either end,\n"
    " see `gleam/queue.Queue`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " last([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " last([1, 2, 3, 4, 5])\n"
    " // -> Ok(5)\n"
    " ```\n"
).
-spec last(list(AQE)) -> {ok, AQE} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("src/gleam/list.gleam", 2177).
?DOC(
    " Return unique combinations of elements in the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " combinations([1, 2, 3], 2)\n"
    " // -> [[1, 2], [1, 3], [2, 3]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " combinations([1, 2, 3, 4], 3)\n"
    " // -> [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]\n"
    " ```\n"
).
-spec combinations(list(AQI), integer()) -> list(list(AQI)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-file("src/gleam/list.gleam", 2195).
-spec do_combination_pairs(list(AQM)) -> list(list({AQM, AQM})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-file("src/gleam/list.gleam", 2214).
?DOC(
    " Return unique pair combinations of elements in the list\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " combination_pairs([1, 2, 3])\n"
    " // -> [#(1, 2), #(1, 3), #(2, 3)]\n"
    " ```\n"
).
-spec combination_pairs(list(AQQ)) -> list({AQQ, AQQ}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    concat(_pipe).

-file("src/gleam/list.gleam", 2246).
?DOC(
    " Transpose rows and columns of the list of lists.\n"
    "\n"
    " Notice: This function is not tail recursive,\n"
    " and thus may exceed stack size if called,\n"
    " with large lists (on target JavaScript).\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " transpose([[1, 2, 3], [101, 102, 103]])\n"
    " // -> [[1, 101], [2, 102], [3, 103]]\n"
    " ```\n"
).
-spec transpose(list(list(AQX))) -> list(list(AQX)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                concat(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-file("src/gleam/list.gleam", 2228).
?DOC(
    " Make a list alternating the elements from the given lists\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " interleave([[1, 2], [101, 102], [201, 202]])\n"
    " // -> [1, 101, 201, 2, 102, 202]\n"
    " ```\n"
).
-spec interleave(list(list(AQT))) -> list(AQT).
interleave(List) ->
    _pipe = transpose(List),
    concat(_pipe).

-file("src/gleam/list.gleam", 2269).
-spec do_shuffle_pair_unwrap(list({float(), ARC}), list(ARC)) -> list(ARC).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-file("src/gleam/list.gleam", 2277).
-spec do_shuffle_by_pair_indexes(list({float(), ARG})) -> list({float(), ARG}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-file("src/gleam/list.gleam", 2296).
?DOC(
    " Takes a list, randomly sorts all items and returns the shuffled list.\n"
    "\n"
    " This function uses `float.random` to decide the order of the elements.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " range(1, 10) |> shuffle()\n"
    " // -> [1, 6, 9, 10, 3, 8, 4, 2, 7, 5]\n"
    " ```\n"
).
-spec shuffle(list(ARJ)) -> list(ARJ).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
