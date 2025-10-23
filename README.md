# 🌊 Squall

A type-safe GraphQL client generator for Gleam, inspired by [squirrel](https://github.com/giacomocavalieri/squirrel).

Squall parses `.gql` files in your project, introspects your GraphQL endpoint, and generates fully type-safe Gleam code with decoders, eliminating runtime errors and keeping your GraphQL queries in sync with your schema.

> **⚠️ Warning**: This project is in early development and may contain bugs. Use at your own risk.

## Features

- Type-safe code generation from GraphQL schema
- Convention over configuration - `.gql` files in `src/**/graphql/` directories
- Schema introspection from GraphQL endpoints
- Supports queries, mutations, and subscriptions
- Client abstraction with authentication and custom headers
- Multiple endpoint support

## Installation

Add squall to your `gleam.toml`:

```toml
[dependencies]
squall = "0.1.0"
```

## Quick Start

### 1. Create a GraphQL query file

Create a file at `src/my_app/graphql/get_character.gql`:

```graphql
query GetCharacter($id: ID!) {
  character(id: $id) {
    id
    name
    status
    species
  }
}
```

### 2. Generate type-safe code

```bash
gleam run -m squall generate https://rickandmortyapi.com/graphql
```

### 3. Use the generated code

```gleam
import squall
import my_app/graphql/get_character

pub fn main() {
  // Create a client
  let client = squall.new_client(
    endpoint: "https://rickandmortyapi.com/graphql",
    headers: []
  )

  // Use the generated function
  case get_character.get_character(client: client, id: "1") {
    Ok(response) -> {
      io.println("Character: " <> response.character.name)
    }
    Error(err) -> {
      io.println("Error: " <> err)
    }
  }
}
```

## Creating a Client

Squall uses a client abstraction to manage your GraphQL endpoint and headers (including authentication):

```gleam
import squall

// Create a client with custom headers
let client = squall.new_client(
  endpoint: "https://api.example.com/graphql",
  headers: [
    #("Authorization", "Bearer your-api-token-here"),
    #("X-Custom-Header", "value")
  ]
)

// Or use the convenience function for bearer token auth
let client = squall.new_client_with_auth(
  endpoint: "https://api.example.com/graphql",
  token: "your-api-token-here"
)

// For public APIs with no authentication
let client = squall.new_client(
  endpoint: "https://rickandmortyapi.com/graphql",
  headers: []
)
```

## How It Works

Squall follows a simple workflow:

1. **Discovery**: Finds all `.gql` files in `src/**/graphql/` directories
2. **Parsing**: Parses GraphQL operations (queries, mutations, subscriptions)
3. **Introspection**: Fetches the GraphQL schema from your endpoint
4. **Type Mapping**: Maps GraphQL types to Gleam types
5. **Code Generation**: Generates:
   - Custom types for responses
   - JSON decoders
   - Type-safe functions with proper parameters

## Project Structure

```
your-project/
├── src/
│   └── my_app/
│       └── graphql/
│           ├── get_user.gql        # Your GraphQL query
│           └── get_user.gleam      # Generated code
└── gleam.toml
```

## Generated Code

For a query like:

```graphql
query GetUser($id: ID!) {
  user(id: $id) {
    id
    name
    email
  }
}
```

Squall generates:

```gleam
pub type GetUserResponse {
  GetUserResponse(
    id: String,
    name: Option(String),
    email: Option(String)
  )
}

pub fn get_user(
  client: squall.Client,
  id: String,
) -> Result(GetUserResponse, String) {
  // HTTP request + JSON decoding implementation
}
```

## Type Mapping

| GraphQL Type | Gleam Type |
|--------------|------------|
| `String` | `String` |
| `Int` | `Int` |
| `Float` | `Float` |
| `Boolean` | `Bool` |
| `ID` | `String` |
| `[Type]` | `List(Type)` |
| `Type` (nullable) | `Option(Type)` |
| `Type!` (non-null) | `Type` |
| Custom Objects | Custom Gleam types |

## CLI Commands

### Generate Code

```bash
# With explicit endpoint
gleam run -m squall generate https://api.example.com/graphql

# Using environment variable
export GRAPHQL_ENDPOINT=https://api.example.com/graphql
gleam run -m squall generate
```

## Architecture

Squall is built with a modular architecture:

- **`discovery`**: Finds and reads `.gql` files
- **`parser`**: Lexes and parses GraphQL operations
- **`schema`**: Introspects and parses GraphQL schemas
- **`type_mapping`**: Maps GraphQL types to Gleam types
- **`codegen`**: Generates Gleam code
- **`error`**: Comprehensive error handling

## Development

### Running Tests

```bash
gleam test
```

### Project Structure

```
squall/
├── src/
│   ├── squall.gleam                    # CLI entry point
│   └── squall/
│       └── internal/
│           ├── discovery.gleam         # File discovery
│           ├── parser.gleam            # GraphQL parser
│           ├── schema.gleam            # Schema introspection
│           ├── type_mapping.gleam      # Type conversion
│           ├── codegen.gleam           # Code generation
│           └── error.gleam             # Error types
├── test/
│   ├── discovery_test.gleam
│   ├── parser_test.gleam
│   ├── schema_test.gleam
│   ├── type_mapping_test.gleam
│   └── codegen_test.gleam
└── birdie_snapshots/                   # Snapshot tests
```

## Roadmap

- [x] Query support
- [x] Mutation support
- [x] Subscription support
- [x] Type-safe code generation
- [x] Schema introspection
- [x] Client abstraction with headers/authentication
- [x] Multiple endpoint support
- [ ] Fragment support
- [ ] Directive handling
- [ ] Custom scalar mapping

## Contributing

Contributions are welcome! Please:

1. Follow TDD principles
2. Add tests for new features
3. Update snapshots when changing code generation
4. Follow Gleam style guidelines

## License

Apache-2.0
