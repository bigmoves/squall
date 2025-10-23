-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/dict.gleam").
-export([size/1, to_list/1, new/0, is_empty/1, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, upsert/3, update/3, fold/3, map_values/2, filter/2, each/2, combine/3]).
-export_type([dict/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type dict(JN, JO) :: any() | {gleam_phantom, JN, JO}.

-file("src/gleam/dict.gleam", 36).
?DOC(
    " Determines the number of key-value pairs in the dict.\n"
    " This function runs in constant time and does not need to iterate the dict.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " new() |> size\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"key\", \"value\") |> size\n"
    " // -> 1\n"
    " ```\n"
).
-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-file("src/gleam/dict.gleam", 80).
?DOC(
    " Converts the dict to a list of 2-element tuples `#(key, value)`, one for\n"
    " each key-value pair in the dict.\n"
    "\n"
    " The tuples in the list have no specific order.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Calling `to_list` on an empty `dict` returns an empty list.\n"
    "\n"
    " ```gleam\n"
    " new() |> to_list\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " The ordering of elements in the resulting list is an implementation detail\n"
    " that should not be relied upon.\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"b\", 1) |> insert(\"a\", 0) |> insert(\"c\", 2) |> to_list\n"
    " // -> [#(\"a\", 0), #(\"b\", 1), #(\"c\", 2)]\n"
    " ```\n"
).
-spec to_list(dict(JX, JY)) -> list({JX, JY}).
to_list(Dict) ->
    maps:to_list(Dict).

-file("src/gleam/dict.gleam", 127).
?DOC(" Creates a fresh dict that contains no values.\n").
-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-file("src/gleam/dict.gleam", 52).
?DOC(
    " Determines whether or not the dict is empty.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " new() |> is_empty\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"b\", 1) |> is_empty\n"
    " // -> False\n"
    " ```\n"
).
-spec is_empty(dict(any(), any())) -> boolean().
is_empty(Dict) ->
    Dict =:= new().

-file("src/gleam/dict.gleam", 152).
?DOC(
    " Fetches a value from a dict for a given key.\n"
    "\n"
    " The dict may not have a value for the key, so the value is wrapped in a\n"
    " `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"a\", 0) |> get(\"a\")\n"
    " // -> Ok(0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"a\", 0) |> get(\"b\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec get(dict(LE, LF), LE) -> {ok, LF} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-file("src/gleam/dict.gleam", 116).
?DOC(
    " Determines whether or not a value present in the dict for a given key.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"a\", 0) |> has_key(\"a\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"a\", 0) |> has_key(\"b\")\n"
    " // -> False\n"
    " ```\n"
).
-spec has_key(dict(KO, any()), KO) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-file("src/gleam/dict.gleam", 177).
?DOC(
    " Inserts a value into the dict with the given key.\n"
    "\n"
    " If the dict already has a value for the given key then the value is\n"
    " replaced with the new value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"a\", 0)\n"
    " // -> from_list([#(\"a\", 0)])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> insert(\"a\", 0) |> insert(\"a\", 5)\n"
    " // -> from_list([#(\"a\", 5)])\n"
    " ```\n"
).
-spec insert(dict(LQ, LR), LQ, LR) -> dict(LQ, LR).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-file("src/gleam/dict.gleam", 92).
-spec fold_list_of_pair(list({KH, KI}), dict(KH, KI)) -> dict(KH, KI).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-file("src/gleam/dict.gleam", 88).
?DOC(
    " Converts a list of 2-element tuples `#(key, value)` to a dict.\n"
    "\n"
    " If two tuples have the same key the last one in the list will be the one\n"
    " that is present in the dict.\n"
).
-spec from_list(list({KC, KD})) -> dict(KC, KD).
from_list(List) ->
    maps:from_list(List).

-file("src/gleam/dict.gleam", 230).
-spec reverse_and_concat(list(TI), list(TI)) -> list(TI).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-file("src/gleam/dict.gleam", 237).
-spec do_keys_acc(list({ND, any()}), list(ND)) -> list(ND).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_keys_acc(Xs, [erlang:element(1, X) | Acc])
    end.

-file("src/gleam/dict.gleam", 220).
?DOC(
    " Gets a list of all keys in a given dict.\n"
    "\n"
    " Dicts are not ordered so the keys are not returned in any specific order. Do\n"
    " not write code that relies on the order keys are returned by this function\n"
    " as it may change in later versions of Gleam or Erlang.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)]) |> keys\n"
    " // -> [\"a\", \"b\"]\n"
    " ```\n"
).
-spec keys(dict(MQ, any())) -> list(MQ).
keys(Dict) ->
    maps:keys(Dict).

-file("src/gleam/dict.gleam", 267).
-spec do_values_acc(list({any(), NT}), list(NT)) -> list(NT).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_values_acc(Xs, [erlang:element(2, X) | Acc])
    end.

-file("src/gleam/dict.gleam", 257).
?DOC(
    " Gets a list of all values in a given dict.\n"
    "\n"
    " Dicts are not ordered so the values are not returned in any specific order. Do\n"
    " not write code that relies on the order values are returned by this function\n"
    " as it may change in later versions of Gleam or Erlang.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)]) |> values\n"
    " // -> [0, 1]\n"
    " ```\n"
).
-spec values(dict(any(), NJ)) -> list(NJ).
values(Dict) ->
    maps:values(Dict).

-file("src/gleam/dict.gleam", 339).
-spec insert_taken(dict(OX, OY), list(OX), dict(OX, OY)) -> dict(OX, OY).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [X | Xs] ->
            insert_taken(Dict, Xs, Insert(Acc, X))
    end.

-file("src/gleam/dict.gleam", 330).
?DOC(
    " Creates a new dict from a given dict, only including any entries for which the\n"
    " keys are in a given list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " |> take([\"b\"])\n"
    " // -> from_list([#(\"b\", 1)])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " |> take([\"a\", \"b\", \"c\"])\n"
    " // -> from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " ```\n"
).
-spec take(dict(OJ, OK), list(OJ)) -> dict(OJ, OK).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-file("src/gleam/dict.gleam", 381).
-spec insert_pair(dict(PW, PX), {PW, PX}) -> dict(PW, PX).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-file("src/gleam/dict.gleam", 385).
-spec fold_inserts(list({QC, QD}), dict(QC, QD)) -> dict(QC, QD).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [X | Xs] ->
            fold_inserts(Xs, insert_pair(Dict, X))
    end.

-file("src/gleam/dict.gleam", 370).
?DOC(
    " Creates a new dict from a pair of given dicts by combining their entries.\n"
    "\n"
    " If there are entries with the same keys in both dicts the entry from the\n"
    " second dict takes precedence.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let a = from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " let b = from_list([#(\"b\", 2), #(\"c\", 3)])\n"
    " merge(a, b)\n"
    " // -> from_list([#(\"a\", 0), #(\"b\", 2), #(\"c\", 3)])\n"
    " ```\n"
).
-spec merge(dict(PG, PH), dict(PG, PH)) -> dict(PG, PH).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-file("src/gleam/dict.gleam", 407).
?DOC(
    " Creates a new dict from a given dict with all the same entries except for the\n"
    " one with a given key, if it exists.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)]) |> delete(\"a\")\n"
    " // -> from_list([#(\"b\", 1)])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)]) |> delete(\"c\")\n"
    " // -> from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " ```\n"
).
-spec delete(dict(QJ, QK), QJ) -> dict(QJ, QK).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-file("src/gleam/dict.gleam", 435).
?DOC(
    " Creates a new dict from a given dict with all the same entries except any with\n"
    " keys found in a given list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)]) |> drop([\"a\"])\n"
    " // -> from_list([#(\"b\", 1)])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)]) |> drop([\"c\"])\n"
    " // -> from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)]) |> drop([\"a\", \"b\", \"c\"])\n"
    " // -> from_list([])\n"
    " ```\n"
).
-spec drop(dict(QV, QW), list(QV)) -> dict(QV, QW).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-file("src/gleam/dict.gleam", 465).
?DOC(
    " Creates a new dict with one entry inserted or updated using a given function.\n"
    "\n"
    " If there was not an entry in the dict for the given key then the function\n"
    " gets `None` as its argument, otherwise it gets `Some(value)`.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let dict = from_list([#(\"a\", 0)])\n"
    " let increment = fn(x) {\n"
    "   case x {\n"
    "     Some(i) -> i + 1\n"
    "     None -> 0\n"
    "   }\n"
    " }\n"
    "\n"
    " upsert(dict, \"a\", increment)\n"
    " // -> from_list([#(\"a\", 1)])\n"
    "\n"
    " upsert(dict, \"b\", increment)\n"
    " // -> from_list([#(\"a\", 0), #(\"b\", 0)])\n"
    " ```\n"
).
-spec upsert(dict(RC, RD), RC, fun((gleam@option:option(RD)) -> RD)) -> dict(RC, RD).
upsert(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-file("src/gleam/dict.gleam", 478).
-spec update(dict(RJ, RK), RJ, fun((gleam@option:option(RK)) -> RK)) -> dict(RJ, RK).
update(Dict, Key, Fun) ->
    upsert(Dict, Key, Fun).

-file("src/gleam/dict.gleam", 486).
-spec do_fold(list({RQ, RR}), RT, fun((RT, RQ, RR) -> RT)) -> RT.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-file("src/gleam/dict.gleam", 518).
?DOC(
    " Combines all entries into a single value by calling a given function on each\n"
    " one.\n"
    "\n"
    " Dicts are not ordered so the values are not returned in any specific order. Do\n"
    " not write code that relies on the order entries are used by this function\n"
    " as it may change in later versions of Gleam or Erlang.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " let dict = from_list([#(\"a\", 1), #(\"b\", 3), #(\"c\", 9)])\n"
    " fold(dict, 0, fn(accumulator, key, value) { accumulator + value })\n"
    " // -> 13\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " import gleam/string\n"
    "\n"
    " let dict = from_list([#(\"a\", 1), #(\"b\", 3), #(\"c\", 9)])\n"
    " fold(dict, \"\", fn(accumulator, key, value) {\n"
    "   string.append(accumulator, key)\n"
    " })\n"
    " // -> \"abc\"\n"
    " ```\n"
).
-spec fold(dict(RU, RV), RY, fun((RY, RU, RV) -> RY)) -> RY.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-file("src/gleam/dict.gleam", 196).
?DOC(
    " Updates all values in a given dict by calling a given function on each key\n"
    " and value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([#(3, 3), #(2, 4)])\n"
    " |> map_values(fn(key, value) { key * value })\n"
    " // -> from_list([#(3, 9), #(2, 8)])\n"
    " ```\n"
).
-spec map_values(dict(MC, MD), fun((MC, MD) -> MG)) -> dict(MC, MG).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-file("src/gleam/dict.gleam", 291).
?DOC(
    " Creates a new dict from a given dict, minus any entries that a given function\n"
    " returns `False` for.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " |> filter(fn(key, value) { value != 0 })\n"
    " // -> from_list([#(\"b\", 1)])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " |> filter(fn(key, value) { True })\n"
    " // -> from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " ```\n"
).
-spec filter(dict(NX, NY), fun((NX, NY) -> boolean())) -> dict(NX, NY).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).

-file("src/gleam/dict.gleam", 550).
?DOC(
    " Calls a function for each key and value in a dict, discarding the return\n"
    " value.\n"
    "\n"
    " Useful for producing a side effect for every item of a dict.\n"
    "\n"
    " ```gleam\n"
    " import gleam/io\n"
    "\n"
    " let dict = from_list([#(\"a\", \"apple\"), #(\"b\", \"banana\"), #(\"c\", \"cherry\")])\n"
    "\n"
    " each(dict, fn(key, value) {\n"
    "   io.println(key <> \" => \" <> value)\n"
    " })\n"
    " // -> Nil\n"
    " // a => apple\n"
    " // b => banana\n"
    " // c => cherry\n"
    " ```\n"
    "\n"
    " The order of elements in the iteration is an implementation detail that\n"
    " should not be relied upon.\n"
).
-spec each(dict(RZ, SA), fun((RZ, SA) -> any())) -> nil.
each(Dict, Fun) ->
    fold(
        Dict,
        nil,
        fun(Nil, K, V) ->
            Fun(K, V),
            Nil
        end
    ).

-file("src/gleam/dict.gleam", 571).
?DOC(
    " Creates a new dict from a pair of given dicts by combining their entries.\n"
    "\n"
    " If there are entries with the same keys in both dicts the given function is\n"
    " used to determine the new value to use in the resulting dict.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let a = from_list([#(\"a\", 0), #(\"b\", 1)])\n"
    " let b = from_list([#(\"a\", 2), #(\"c\", 3)])\n"
    " combine(a, b, fn(one, other) { one + other })\n"
    " // -> from_list([#(\"a\", 2), #(\"b\", 1), #(\"c\", 3)])\n"
    " ```\n"
).
-spec combine(dict(SE, SF), dict(SE, SF), fun((SF, SF) -> SF)) -> dict(SE, SF).
combine(Dict, Other, Fun) ->
    fold(Dict, Other, fun(Acc, Key, Value) -> case get(Acc, Key) of
                {ok, Other_value} ->
                    insert(Acc, Key, Fun(Value, Other_value));

                {error, _} ->
                    insert(Acc, Key, Value)
            end end).
