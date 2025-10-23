-module(gleam@json).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/json.gleam").
-export([decode_bits/2, decode/2, to_string/1, to_string_builder/1, string/1, bool/1, int/1, float/1, null/0, nullable/2, object/1, preprocessed_array/1, array/2]).
-export_type([json/0, decode_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type json() :: any().

-type decode_error() :: unexpected_end_of_input |
    {unexpected_byte, binary(), integer()} |
    {unexpected_sequence, binary(), integer()} |
    {unexpected_format, list(gleam@dynamic:decode_error())}.

-file("src/gleam/json.gleam", 87).
?DOC(
    " Decode a JSON bit string into dynamically typed data which can be decoded\n"
    " into typed data with the `gleam/dynamic` module.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > decode_bits(<<\"[1,2,3]\">>, dynamic.list(of: dynamic.int))\n"
    " Ok([1, 2, 3])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > decode_bits(<<\"[\">>, dynamic.list(of: dynamic.int))\n"
    " Error(UnexpectedEndOfInput)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > decode_bits(\"<<1\">>, dynamic.string)\n"
    " Error(UnexpectedFormat([dynamic.DecodeError(\"String\", \"Int\", [])]))\n"
    " ```\n"
).
-spec decode_bits(
    bitstring(),
    fun((gleam@dynamic:dynamic_()) -> {ok, GYM} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, GYM} | {error, decode_error()}.
decode_bits(Json, Decoder) ->
    gleam@result:then(
        gleam_json_ffi:decode(Json),
        fun(Dynamic_value) -> _pipe = Decoder(Dynamic_value),
            gleam@result:map_error(
                _pipe,
                fun(Field@0) -> {unexpected_format, Field@0} end
            ) end
    ).

-file("src/gleam/json.gleam", 45).
-spec do_decode(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, GYI} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, GYI} | {error, decode_error()}.
do_decode(Json, Decoder) ->
    Bits = gleam_stdlib:identity(Json),
    decode_bits(Bits, Decoder).

-file("src/gleam/json.gleam", 37).
?DOC(
    " Decode a JSON string into dynamically typed data which can be decoded into\n"
    " typed data with the `gleam/dynamic` module.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > decode(\"[1,2,3]\", dynamic.list(of: dynamic.int))\n"
    " Ok([1, 2, 3])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > decode(\"[\", dynamic.list(of: dynamic.int))\n"
    " Error(UnexpectedEndOfInput)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > decode(\"1\", dynamic.string)\n"
    " Error(UnexpectedFormat([dynamic.DecodeError(\"String\", \"Int\", [])]))\n"
    " ```\n"
).
-spec decode(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, GYE} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, GYE} | {error, decode_error()}.
decode(Json, Decoder) ->
    do_decode(Json, Decoder).

-file("src/gleam/json.gleam", 120).
?DOC(
    " Convert a JSON value into a string.\n"
    "\n"
    " Where possible prefer the `to_string_builder` function as it is faster than\n"
    " this function, and BEAM VM IO is optimised for sending `StringBuilder` data.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(array([1, 2, 3], of: int))\n"
    " \"[1,2,3]\"\n"
    " ```\n"
).
-spec to_string(json()) -> binary().
to_string(Json) ->
    gleam_json_ffi:json_to_string(Json).

-file("src/gleam/json.gleam", 141).
?DOC(
    " Convert a JSON value into a string builder.\n"
    "\n"
    " Where possible prefer this function to the `to_string` function as it is\n"
    " slower than this function, and BEAM VM IO is optimised for sending\n"
    " `StringBuilder` data.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string_builder(array([1, 2, 3], of: int))\n"
    " string_builder.from_string(\"[1,2,3]\")\n"
    " ```\n"
).
-spec to_string_builder(json()) -> gleam@string_builder:string_builder().
to_string_builder(Json) ->
    gleam_json_ffi:json_to_iodata(Json).

-file("src/gleam/json.gleam", 158).
?DOC(
    " Encode a string into JSON, using normal JSON escaping.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(string(\"Hello!\"))\n"
    " \"\\\"Hello!\\\"\"\n"
    " ```\n"
).
-spec string(binary()) -> json().
string(Input) ->
    gleam_json_ffi:string(Input).

-file("src/gleam/json.gleam", 175).
?DOC(
    " Encode a bool into JSON.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(bool(False))\n"
    " \"false\"\n"
    " ```\n"
).
-spec bool(boolean()) -> json().
bool(Input) ->
    gleam_json_ffi:bool(Input).

-file("src/gleam/json.gleam", 192).
?DOC(
    " Encode an int into JSON.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(int(50))\n"
    " \"50\"\n"
    " ```\n"
).
-spec int(integer()) -> json().
int(Input) ->
    gleam_json_ffi:int(Input).

-file("src/gleam/json.gleam", 209).
?DOC(
    " Encode a float into JSON.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(float(4.7))\n"
    " \"4.7\"\n"
    " ```\n"
).
-spec float(float()) -> json().
float(Input) ->
    gleam_json_ffi:float(Input).

-file("src/gleam/json.gleam", 226).
?DOC(
    " The JSON value null.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(null())\n"
    " \"null\"\n"
    " ```\n"
).
-spec null() -> json().
null() ->
    gleam_json_ffi:null().

-file("src/gleam/json.gleam", 248).
?DOC(
    " Encode an optional value into JSON, using null if it is the `None` variant.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(nullable(Some(50), of: int))\n"
    " \"50\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > to_string(nullable(None, of: int))\n"
    " \"null\"\n"
    " ```\n"
).
-spec nullable(gleam@option:option(GYS), fun((GYS) -> json())) -> json().
nullable(Input, Inner_type) ->
    case Input of
        {some, Value} ->
            Inner_type(Value);

        none ->
            null()
    end.

-file("src/gleam/json.gleam", 267).
?DOC(
    " Encode a list of key-value pairs into a JSON object.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(object([\n"
    "   #(\"game\", string(\"Pac-Man\")),\n"
    "   #(\"score\", int(3333360)),\n"
    " ]))\n"
    " \"{\\\"game\\\":\\\"Pac-Mac\\\",\\\"score\\\":3333360}\"\n"
    " ```\n"
).
-spec object(list({binary(), json()})) -> json().
object(Entries) ->
    gleam_json_ffi:object(Entries).

-file("src/gleam/json.gleam", 299).
?DOC(
    " Encode a list of JSON values into a JSON array.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(preprocessed_array([int(1), float(2.0), string(\"3\")]))\n"
    " \"[1, 2.0, \\\"3\\\"]\"\n"
    " ```\n"
).
-spec preprocessed_array(list(json())) -> json().
preprocessed_array(From) ->
    gleam_json_ffi:array(From).

-file("src/gleam/json.gleam", 284).
?DOC(
    " Encode a list into a JSON array.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > to_string(array([1, 2, 3], of: int))\n"
    " \"[1, 2, 3]\"\n"
    " ```\n"
).
-spec array(list(GYW), fun((GYW) -> json())) -> json().
array(Entries, Inner_type) ->
    _pipe = Entries,
    _pipe@1 = gleam@list:map(_pipe, Inner_type),
    preprocessed_array(_pipe@1).
