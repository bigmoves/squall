-record(schema, {
    query_type :: gleam@option:option(binary()),
    mutation_type :: gleam@option:option(binary()),
    subscription_type :: gleam@option:option(binary()),
    types :: gleam@dict:dict(binary(), squall@internal@schema:type())
}).
