-record(object_type, {
    name :: binary(),
    fields :: list(squall@internal@schema:field()),
    description :: gleam@option:option(binary())
}).
