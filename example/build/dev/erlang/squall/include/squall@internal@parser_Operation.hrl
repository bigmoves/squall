-record(operation, {
    operation_type :: squall@internal@parser:operation_type(),
    name :: gleam@option:option(binary()),
    variables :: list(squall@internal@parser:variable()),
    selections :: list(squall@internal@parser:selection())
}).
