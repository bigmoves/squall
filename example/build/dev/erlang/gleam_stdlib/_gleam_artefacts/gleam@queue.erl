-module(gleam@queue).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/queue.gleam").
-export([new/0, from_list/1, to_list/1, is_empty/1, length/1, push_back/2, push_front/2, pop_back/1, pop_front/1, reverse/1, is_logically_equal/3, is_equal/2]).
-export_type([queue/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque queue(EUL) :: {queue, list(EUL), list(EUL)}.

-file("src/gleam/queue.gleam", 22).
?DOC(" Creates a fresh queue that contains no values.\n").
-spec new() -> queue(any()).
new() ->
    {queue, [], []}.

-file("src/gleam/queue.gleam", 38).
?DOC(
    " Converts a list of elements into a queue of the same elements in the same\n"
    " order. The first element in the list becomes the front element in the queue.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3] |> from_list |> length\n"
    " // -> 3\n"
    " ```\n"
).
-spec from_list(list(EUO)) -> queue(EUO).
from_list(List) ->
    {queue, [], List}.

-file("src/gleam/queue.gleam", 54).
?DOC(
    " Converts a queue of elements into a list of the same elements in the same\n"
    " order. The front element in the queue becomes the first element in the list.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " new() |> push_back(1) |> push_back(2) |> to_list\n"
    " // -> [1, 2]\n"
    " ```\n"
).
-spec to_list(queue(EUR)) -> list(EUR).
to_list(Queue) ->
    _pipe = erlang:element(3, Queue),
    lists:append(_pipe, lists:reverse(erlang:element(2, Queue))).

-file("src/gleam/queue.gleam", 80).
?DOC(
    " Determines whether or not the queue is empty.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> from_list |> is_empty\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1] |> from_list |> is_empty\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2] |> from_list |> is_empty\n"
    " // -> False\n"
    " ```\n"
).
-spec is_empty(queue(any())) -> boolean().
is_empty(Queue) ->
    (erlang:element(2, Queue) =:= []) andalso (erlang:element(3, Queue) =:= []).

-file("src/gleam/queue.gleam", 106).
?DOC(
    " Counts the number of elements in a given queue.\n"
    "\n"
    " This function has to traverse the queue to determine the number of elements,\n"
    " so it runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " length(from_list([]))\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length(from_list([1]))\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length(from_list([1, 2]))\n"
    " // -> 2\n"
    " ```\n"
).
-spec length(queue(any())) -> integer().
length(Queue) ->
    erlang:length(erlang:element(2, Queue)) + erlang:length(
        erlang:element(3, Queue)
    ).

-file("src/gleam/queue.gleam", 119).
?DOC(
    " Pushes an element onto the back of the queue.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2] |> from_list |> push_back(3) |> to_list\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec push_back(queue(EUY), EUY) -> queue(EUY).
push_back(Queue, Item) ->
    {queue, [Item | erlang:element(2, Queue)], erlang:element(3, Queue)}.

-file("src/gleam/queue.gleam", 132).
?DOC(
    " Pushes an element onto the front of the queue.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " [0, 0] |> from_list |> push_front(1) |> to_list\n"
    " // -> [1, 0, 0]\n"
    " ```\n"
).
-spec push_front(queue(EVB), EVB) -> queue(EVB).
push_front(Queue, Item) ->
    {queue, erlang:element(2, Queue), [Item | erlang:element(3, Queue)]}.

-file("src/gleam/queue.gleam", 164).
?DOC(
    " Gets the last element from the queue, returning the\n"
    " element and a new queue without that element.\n"
    "\n"
    " This function typically runs in constant time, but will occasionally run in\n"
    " linear time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_back(0)\n"
    " |> push_back(1)\n"
    " |> pop_back\n"
    " // -> Ok(#(1, push_front(new(), 0)))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_front(0)\n"
    " |> pop_back\n"
    " // -> Ok(#(0, new()))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> pop_back\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec pop_back(queue(EVE)) -> {ok, {EVE, queue(EVE)}} | {error, nil}.
pop_back(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, [], Out} ->
            pop_back({queue, lists:reverse(Out), []});

        {queue, [First | Rest], Out@1} ->
            Queue@1 = {queue, Rest, Out@1},
            {ok, {First, Queue@1}}
    end.

-file("src/gleam/queue.gleam", 203).
?DOC(
    " Gets the first element from the queue, returning the\n"
    " element and a new queue without that element.\n"
    "\n"
    " This function typically runs in constant time, but will occasionally run in\n"
    " linear time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_front(1)\n"
    " |> push_front(0)\n"
    " |> pop_front\n"
    " // -> Ok(#(0, push_back(new(), 1)))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_back(0)\n"
    " |> pop_front\n"
    " // -> Ok(#(0, new()))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> pop_back\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec pop_front(queue(EVJ)) -> {ok, {EVJ, queue(EVJ)}} | {error, nil}.
pop_front(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, In, []} ->
            pop_front({queue, [], lists:reverse(In)});

        {queue, In@1, [First | Rest]} ->
            Queue@1 = {queue, In@1, Rest},
            {ok, {First, Queue@1}}
    end.

-file("src/gleam/queue.gleam", 236).
?DOC(
    " Creates a new queue from a given queue containing the same elements, but in\n"
    " the opposite order.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> from_list |> reverse |> to_list\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1] |> from_list |> reverse |> to_list\n"
    " // -> [1]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2] |> from_list |> reverse |> to_list\n"
    " // -> [2, 1]\n"
    " ```\n"
).
-spec reverse(queue(EVO)) -> queue(EVO).
reverse(Queue) ->
    {queue, erlang:element(3, Queue), erlang:element(2, Queue)}.

-file("src/gleam/queue.gleam", 240).
-spec check_equal(
    list(EVR),
    list(EVR),
    list(EVR),
    list(EVR),
    fun((EVR, EVR) -> boolean())
) -> boolean().
check_equal(Xs, X_tail, Ys, Y_tail, Eq) ->
    case {Xs, X_tail, Ys, Y_tail} of
        {[], [], [], []} ->
            true;

        {[X | Xs@1], _, [Y | Ys@1], _} ->
            case Eq(X, Y) of
                false ->
                    false;

                true ->
                    check_equal(Xs@1, X_tail, Ys@1, Y_tail, Eq)
            end;

        {[], [_ | _], _, _} ->
            check_equal(lists:reverse(X_tail), [], Ys, Y_tail, Eq);

        {_, _, [], [_ | _]} ->
            check_equal(Xs, X_tail, lists:reverse(Y_tail), [], Eq);

        {_, _, _, _} ->
            false
    end.

-file("src/gleam/queue.gleam", 271).
?DOC(
    " Checks whether two queues have equal elements in the same order, where the\n"
    " equality of elements is determined by a given equality checking function.\n"
    "\n"
    " This function is useful as the internal representation may be different for\n"
    " two queues with the same elements in the same order depending on how they\n"
    " were constructed, so the equality operator `==` may return surprising\n"
    " results.\n"
    "\n"
    " This function runs in linear time multiplied by the time taken by the\n"
    " element equality checking function.\n"
).
-spec is_logically_equal(queue(EVW), queue(EVW), fun((EVW, EVW) -> boolean())) -> boolean().
is_logically_equal(A, B, Element_is_equal) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        Element_is_equal
    ).

-file("src/gleam/queue.gleam", 288).
?DOC(
    " Checks whether two queues have the same elements in the same order.\n"
    "\n"
    " This function is useful as the internal representation may be different for\n"
    " two queues with the same elements in the same order depending on how they\n"
    " were constructed, so the equality operator `==` may return surprising\n"
    " results.\n"
    "\n"
    " This function runs in linear time.\n"
).
-spec is_equal(queue(EVZ), queue(EVZ)) -> boolean().
is_equal(A, B) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        fun(A@1, B@1) -> A@1 =:= B@1 end
    ).
