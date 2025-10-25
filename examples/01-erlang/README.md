# Squall Erlang Example

This example demonstrates using Squall-generated GraphQL clients on the **Erlang/OTP** target.

## What's Inside

- `erlang_example.gleam` - Multi-field query example
- `example_with_vars.gleam` - Query with GraphQL variables
- `src/graphql/*.gql` - GraphQL query definitions
- `src/graphql/*.gleam` - Generated type-safe Gleam code

## HTTP Adapter

This example uses the **Erlang adapter** which uses `gleam_httpc` for HTTP requests:

```gleam
import squall

let client = squall.new_erlang_client("https://rickandmortyapi.com/graphql", [])
```

## Running

```bash
# Build the project
gleam build

# Run the main example
gleam run -m erlang_example

# Run the variables example
gleam run -m example_with_vars
```

## Regenerating GraphQL Code

If you modify the `.gql` files, regenerate the code:

```bash
# From the squall root directory
cd ../..
gleam run -m squall generate https://rickandmortyapi.com/graphql
```

This will update the `.gleam` files in `src/graphql/`.

## Features Demonstrated

✅ **Erlang/OTP Runtime** - Uses `gleam_httpc` HTTP client
✅ **Type Safety** - Fully typed based on GraphQL schema
✅ **Nested Objects** - Automatic decoder generation
✅ **Optional Fields** - Proper `Option` type handling
✅ **Variables** - Type-safe GraphQL variables
✅ **JSON Serialization** - Convert responses back to JSON
