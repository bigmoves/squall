-record(input_object_type, {
    name :: binary(),
    input_fields :: list(squall@internal@schema:input_value()),
    description :: gleam@option:option(binary())
}).
