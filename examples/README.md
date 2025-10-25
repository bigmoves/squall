# Squall Examples

Welcome to the Squall examples! These demonstrate how to use Squall's **isomorphic GraphQL client generator** with the Rick and Morty API.

## 🌊 What is Squall?

Squall generates **type-safe GraphQL clients** for Gleam that work on **any target** - Erlang, JavaScript (Browser), and JavaScript (Node.js). Write `.gql` files, and Squall generates fully-typed Gleam code with proper types, decoders, and serializers.

## Examples Overview

### [01-erlang](./01-erlang/) - Erlang/OTP Target

Demonstrates Squall on the Erlang/OTP runtime using `gleam_httpc`.

```bash
cd 01-erlang
gleam run -m erlang_example
```

**Perfect for:** Backend services, servers, distributed systems

### [02-javascript](./02-javascript/) - JavaScript Target

Demonstrates Squall on JavaScript (Node.js/Browser) using the Fetch API.

```bash
cd 02-javascript
gleam run -m javascript_example
```

**Perfect for:** Frontend apps, serverless functions, Deno

### [03-isomorphic](./03-isomorphic/) - Cross-Platform

Demonstrates writing code that works on **both** Erlang and JavaScript!

```bash
cd 03-isomorphic

# Run on Erlang
gleam run -m isomorphic_example

# Run on JavaScript
gleam run --target javascript -m isomorphic_example
```

**Perfect for:** Full-stack apps, shared libraries, universal code

## Quick Start

### 1. Generate GraphQL Code

From the Squall root directory:

```bash
gleam run -m squall generate https://rickandmortyapi.com/graphql
```

This discovers all `.gql` files in the examples and generates type-safe Gleam code.

### 2. Choose Your Target

Pick the example that matches your use case:

| Target | Example | Use Case |
|--------|---------|----------|
| **Erlang** | `01-erlang` | Backend, servers |
| **JavaScript** | `02-javascript` | Frontend, Node.js |
| **Both** | `03-isomorphic` | Full-stack apps |

### 3. Run It!

```bash
cd <example-folder>
gleam run
```

## The Key Difference

The **only difference** between Erlang and JavaScript code is the client creation:

```gleam
// Erlang
let client = squall.new_erlang_client(endpoint, headers)

// JavaScript
let client = squall.new_javascript_client(endpoint, headers)

// Cross-platform (uses @target conditionals)
@target(erlang)
fn create_client() -> squall.Client {
  squall.new_erlang_client(endpoint, headers)
}

@target(javascript)
fn create_client() -> squall.Client {
  squall.new_javascript_client(endpoint, headers)
}
```

**Everything else is identical!** The same generated GraphQL code works everywhere.

## API Endpoint

All examples use the public Rick and Morty GraphQL API:

```
https://rickandmortyapi.com/graphql
```

No API key required!

## GraphQL Queries

Each example includes the same `.gql` files:

- **`get_character.gql`** - Fetch a single character by ID
- **`get_characters.gql`** - Fetch a list of characters
- **`multi_query.gql`** - Multiple fields in one query
- **`multi_query_with_vars.gql`** - Query with GraphQL variables

## Features Demonstrated

✅ **Type Safety** - All queries fully typed from GraphQL schema
✅ **Isomorphic** - Same code works on Erlang and JavaScript
✅ **Variables** - Type-safe GraphQL variable handling
✅ **Nested Objects** - Automatic decoder generation for complex types
✅ **Optional Fields** - Proper `Option` type handling
✅ **JSON Serialization** - Convert responses to/from JSON
✅ **HTTP Adapters** - Platform-specific HTTP implementations

## Project Structure

```
examples/
├── README.md                    # This file
├── 01-erlang/                   # Erlang example
│   ├── gleam.toml
│   ├── README.md
│   └── src/
│       ├── erlang_example.gleam
│       ├── example_with_vars.gleam
│       └── graphql/
│           ├── *.gql           # GraphQL queries
│           └── *.gleam         # Generated code
├── 02-javascript/               # JavaScript example
│   ├── gleam.toml
│   ├── README.md
│   └── src/
│       ├── javascript_example.gleam
│       └── graphql/
│           ├── *.gql
│           └── *.gleam
└── 03-isomorphic/              # Cross-platform example
    ├── gleam.toml
    ├── README.md
    └── src/
        ├── isomorphic_example.gleam
        └── graphql/
            ├── *.gql
            └── *.gleam
```

## Next Steps

1. **Explore the examples** - Start with the one matching your target
2. **Read the individual READMEs** - Each has detailed instructions
3. **Modify the queries** - Edit `.gql` files and regenerate
4. **Try your own API** - Point Squall at your GraphQL endpoint!

## Learn More

- [Squall Documentation](../../CLAUDE.md)
- [Rick and Morty GraphQL API](https://rickandmortyapi.com/documentation/#graphql)
- [Gleam Language](https://gleam.run)

Happy querying! 🌊
