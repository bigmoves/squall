-record(multi_query_with_vars_response, {
    characters :: gleam@option:option(graphql@multi_query_with_vars:characters()),
    location :: gleam@option:option(graphql@multi_query_with_vars:location()),
    episodes_by_ids :: gleam@option:option(list(graphql@multi_query_with_vars:episode()))
}).
