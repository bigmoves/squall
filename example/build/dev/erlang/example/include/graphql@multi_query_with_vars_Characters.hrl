-record(characters, {
    info :: gleam@option:option(graphql@multi_query_with_vars:info()),
    results :: gleam@option:option(list(graphql@multi_query_with_vars:character()))
}).
