-record(multi_query_response, {
    characters :: gleam@option:option(graphql@multi_query:characters()),
    location :: gleam@option:option(graphql@multi_query:location()),
    episodes_by_ids :: gleam@option:option(list(graphql@multi_query:episode()))
}).
