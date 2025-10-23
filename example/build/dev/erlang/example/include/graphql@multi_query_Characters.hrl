-record(characters, {
    info :: gleam@option:option(graphql@multi_query:info()),
    results :: gleam@option:option(list(graphql@multi_query:character()))
}).
