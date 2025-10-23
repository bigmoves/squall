-record(field_selection, {
    name :: binary(),
    alias :: gleam@option:option(binary()),
    arguments :: list(squall@internal@parser:argument()),
    selections :: list(squall@internal@parser:selection())
}).
