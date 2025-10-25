# Squall JavaScript Example

This example demonstrates using Squall-generated GraphQL clients on the **JavaScript** target (Node.js or browser).

## What's Inside

- `javascript_example.gleam` - Multi-field query example using Fetch API
- `src/graphql/*.gql` - GraphQL query definitions
- `src/graphql/*.gleam` - Generated type-safe Gleam code (same as Erlang!)

## HTTP Adapter

This example uses the **JavaScript adapter** which uses the Fetch API for HTTP requests:

```gleam
import squall

let client = squall.new_javascript_client("https://rickandmortyapi.com/graphql", [])
```

The Fetch API works in:
- **Node.js** v18+ (built-in)
- **Browsers** (all modern browsers)
- **Deno** (built-in)

## Running

### Node.js (Recommended)

```bash
# Build for JavaScript target
gleam build

# Run the example
gleam run -m javascript_example
```

### Browser

1. Build the JavaScript bundle:
   ```bash
   gleam build --target javascript
   ```

2. The compiled JavaScript will be in `build/dev/javascript/`

3. Include it in an HTML file or use with a bundler (Vite, Webpack, etc.)

## Requirements

- **Node.js** v18.0.0 or later (for native Fetch API support)
- Or any modern browser

## Regenerating GraphQL Code

If you modify the `.gql` files, regenerate the code:

```bash
# From the squall root directory
cd ../..
gleam run -m squall generate https://rickandmortyapi.com/graphql
```

This will update the `.gleam` files in `src/graphql/`.

## Key Differences from Erlang Example

The **only difference** between this and the Erlang example is the client creation:

```gleam
// JavaScript
squall.new_javascript_client(endpoint, headers)

// vs Erlang
squall.new_erlang_client(endpoint, headers)
```

**Everything else is identical!** The same generated GraphQL code works on both targets.

## Features Demonstrated

✅ **JavaScript Runtime** - Uses Fetch API (Node.js/Browser)
✅ **Type Safety** - Fully typed based on GraphQL schema
✅ **Isomorphic Code** - Same generated code as Erlang example
✅ **Nested Objects** - Automatic decoder generation
✅ **Optional Fields** - Proper `Option` type handling
✅ **JSON Serialization** - Convert responses back to JSON
