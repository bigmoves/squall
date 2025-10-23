-module(gleam@http@cookie).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/http/cookie.gleam").
-export([defaults/1, parse/1, set_header/3]).
-export_type([same_site_policy/0, attributes/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type same_site_policy() :: lax | strict | none.

-type attributes() :: {attributes,
        gleam@option:option(integer()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        boolean(),
        boolean(),
        gleam@option:option(same_site_policy())}.

-file("src/gleam/http/cookie.gleam", 18).
-spec same_site_to_string(same_site_policy()) -> binary().
same_site_to_string(Policy) ->
    case Policy of
        lax ->
            <<"Lax"/utf8>>;

        strict ->
            <<"Strict"/utf8>>;

        none ->
            <<"None"/utf8>>
    end.

-file("src/gleam/http/cookie.gleam", 41).
?DOC(
    " Helper to create sensible default attributes for a set cookie.\n"
    "\n"
    " https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#Attributes\n"
).
-spec defaults(gleam@http:scheme()) -> attributes().
defaults(Scheme) ->
    {attributes,
        none,
        none,
        {some, <<"/"/utf8>>},
        Scheme =:= https,
        true,
        {some, lax}}.

-file("src/gleam/http/cookie.gleam", 117).
-spec check_token(binary()) -> {ok, nil} | {error, nil}.
check_token(Token) ->
    case gleam@string:pop_grapheme(Token) of
        {error, nil} ->
            {ok, nil};

        {ok, {<<" "/utf8>>, _}} ->
            {error, nil};

        {ok, {<<"\t"/utf8>>, _}} ->
            {error, nil};

        {ok, {<<"\r"/utf8>>, _}} ->
            {error, nil};

        {ok, {<<"\n"/utf8>>, _}} ->
            {error, nil};

        {ok, {<<"\f"/utf8>>, _}} ->
            {error, nil};

        {ok, {_, Rest}} ->
            check_token(Rest)
    end.

-file("src/gleam/http/cookie.gleam", 99).
?DOC(
    " Parse a list of cookies from a header string. Any malformed cookies will be\n"
    " discarded.\n"
).
-spec parse(binary()) -> list({binary(), binary()}).
parse(Cookie_string) ->
    Re@1 = case gleam@regex:from_string(<<"[,;]"/utf8>>) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"gleam/http/cookie"/utf8>>,
                        function => <<"parse"/utf8>>,
                        line => 100,
                        value => _assert_fail,
                        start => 2697,
                        'end' => 2742,
                        pattern_start => 2708,
                        pattern_end => 2714})
    end,
    _pipe = gleam@regex:split(Re@1, Cookie_string),
    gleam@list:filter_map(
        _pipe,
        fun(Pair) ->
            case gleam@string:split_once(gleam@string:trim(Pair), <<"="/utf8>>) of
                {ok, {<<""/utf8>>, _}} ->
                    {error, nil};

                {ok, {Key, Value}} ->
                    Key@1 = gleam@string:trim(Key),
                    Value@1 = gleam@string:trim(Value),
                    gleam@result:then(
                        check_token(Key@1),
                        fun(_) ->
                            gleam@result:then(
                                check_token(Value@1),
                                fun(_) -> {ok, {Key@1, Value@1}} end
                            )
                        end
                    );

                {error, nil} ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/http/cookie.gleam", 54).
-spec cookie_attributes_to_list(attributes()) -> list(list(binary())).
cookie_attributes_to_list(Attributes) ->
    {attributes, Max_age, Domain, Path, Secure, Http_only, Same_site} = Attributes,
    _pipe = [case Max_age of
            {some, 0} ->
                {some, [<<"Expires=Thu, 01 Jan 1970 00:00:00 GMT"/utf8>>]};

            _ ->
                none
        end, gleam@option:map(
            Max_age,
            fun(Max_age@1) ->
                [<<"Max-Age="/utf8>>, gleam@int:to_string(Max_age@1)]
            end
        ), gleam@option:map(
            Domain,
            fun(Domain@1) -> [<<"Domain="/utf8>>, Domain@1] end
        ), gleam@option:map(Path, fun(Path@1) -> [<<"Path="/utf8>>, Path@1] end), case Secure of
            true ->
                {some, [<<"Secure"/utf8>>]};

            false ->
                none
        end, case Http_only of
            true ->
                {some, [<<"HttpOnly"/utf8>>]};

            false ->
                none
        end, gleam@option:map(
            Same_site,
            fun(Same_site@1) ->
                [<<"SameSite="/utf8>>, same_site_to_string(Same_site@1)]
            end
        )],
    gleam@list:filter_map(
        _pipe,
        fun(_capture) -> gleam@option:to_result(_capture, nil) end
    ).

-file("src/gleam/http/cookie.gleam", 90).
-spec set_header(binary(), binary(), attributes()) -> binary().
set_header(Name, Value, Attributes) ->
    _pipe = [[Name, <<"="/utf8>>, Value] |
        cookie_attributes_to_list(Attributes)],
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(_capture) -> gleam@string:join(_capture, <<""/utf8>>) end
    ),
    gleam@string:join(_pipe@1, <<"; "/utf8>>).
