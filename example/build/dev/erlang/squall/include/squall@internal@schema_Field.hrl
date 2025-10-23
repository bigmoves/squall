-record(field, {
    name :: binary(),
    type_ref :: squall@internal@schema:type_ref(),
    args :: list(squall@internal@schema:input_value()),
    description :: gleam@option:option(binary())
}).
