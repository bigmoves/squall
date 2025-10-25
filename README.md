# 🌊 Squall

A type-safe **isomorphic** GraphQL client generator for Gleam.

Squall parses `.gql` files in your project, introspects your GraphQL endpoint, and generates fully type-safe Gleam code that works on **both Erlang and JavaScript** targets. Write your queries once, run them anywhere!

> **⚠️ Warning**: This project is in early development and may contain bugs. Also hasn't been published yet.

## Features

✨ **Isomorphic** - Works on Erlang (backend) and JavaScript (browser/Node.js)
- Type-safe code generation from GraphQL schema
- Convention over configuration - `.gql` files in `src/**/graphql/` directories
- Schema introspection from GraphQL endpoints
- Supports queries, mutations, and subscriptions
- Target-specific HTTP adapters (gleam_httpc for Erlang, Fetch API for JavaScript)
- Client abstraction with authentication and custom headers

## Installation

Add squall to your `gleam.toml`:

```toml
[dependencies]
squall = "0.1.0"

# If using JavaScript target, also add:
gleam_javascript = ">= 0.3.0 and < 2.0.0"
```

The `gleam_javascript` dependency provides Promise support needed for async operations on the JavaScript target.

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
  // Create a client for your target
  // On Erlang:
  let client = squall.new_erlang_client(
    "https://rickandmortyapi.com/graphql",
    []
  )

  // On JavaScript:
  // let client = squall.new_javascript_client(
  //   "https://rickandmortyapi.com/graphql",
  //   []
  // )

  // Use the generated function - works on both targets!
  case get_character.get_character(client, "1") {
    Ok(response) -> {
      io.println("Character: " <> response.character.name)
    }
    Error(err) -> {
      io.println("Error: " <> err)
    }
  }
}
```

**Note**: On JavaScript, the generated function returns a `Promise(Result(...))` instead of `Result(...)`. See the [examples](./examples/) for how to handle both targets.

## Creating a Client

Squall provides target-specific client constructors:

### Erlang Target

```gleam
import squall

// Basic client
let client = squall.new_erlang_client(
  "https://api.example.com/graphql",
  [#("X-Custom-Header", "value")]
)

// With bearer token authentication
let client = squall.new_erlang_client_with_auth(
  "https://api.example.com/graphql",
  "your-api-token-here"
)
```

### JavaScript Target

```gleam
import squall

// Basic client
let client = squall.new_javascript_client(
  "https://api.example.com/graphql",
  [#("X-Custom-Header", "value")]
)

// With bearer token authentication
let client = squall.new_javascript_client_with_auth(
  "https://api.example.com/graphql",
  "your-api-token-here"
)
```

### Isomorphic Code

For code that works on both targets, use `@target()` conditionals:

```gleam
import squall

@target(erlang)
fn create_client() -> squall.Client {
  squall.new_erlang_client("https://api.example.com/graphql", [])
}

@target(javascript)
fn create_client() -> squall.Client {
  squall.new_javascript_client("https://api.example.com/graphql", [])
}

pub fn main() {
  let client = create_client()  // Works on both targets!
  // ... use the client
}
```

## Isomorphic Architecture

Squall's isomorphic design means you write your GraphQL queries once, and they work on both Erlang and JavaScript:

### What's Different Between Targets?

| Aspect | Erlang | JavaScript |
|--------|--------|------------|
| **HTTP Client** | `gleam_httpc` | Fetch API |
| **Return Type** | `Result(T, String)` | `Promise(Result(T, String))` |
| **Execution** | Synchronous | Asynchronous |
| **Client Constructor** | `new_erlang_client()` | `new_javascript_client()` |

### What's the Same?

✅ **GraphQL queries** - Same `.gql` files
✅ **Generated types** - Identical response types
✅ **Generated decoders** - Same JSON parsing logic
✅ **Business logic** - Share response handling code
✅ **Generated functions** - Same function signatures (except return type)

The only code you need to write differently is:
1. Client creation (use `@target()` conditionals)
2. Promise handling on JavaScript (use `promise.await()`)

Everything else is truly isomorphic!

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

## Examples

Check out the [examples](./examples/) directory for complete working examples:

- **[01-erlang](./examples/01-erlang/)** - Erlang/OTP backend example
- **[02-javascript](./examples/02-javascript/)** - JavaScript (Node.js/Browser) example
- **[03-isomorphic](./examples/03-isomorphic/)** - Cross-platform example that runs on both!

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

Squall generates type-safe Gleam code with:

**Response Types:**
```gleam
pub type GetUserResponse {
  GetUserResponse(user: Option(User))
}

pub type User {
  User(
    id: String,
    name: Option(String),
    email: Option(String)
  )
}
```

**Decoders:**
```gleam
fn get_user_response_decoder() -> decode.Decoder(GetUserResponse) {
  // Auto-generated JSON decoder
}
```

**Query Function:**
```gleam
// On Erlang - returns Result
pub fn get_user(
  client: squall.Client,
  id: String,
) -> Result(GetUserResponse, String) {
  squall.execute_query(client, query, variables, decoder)
}

// On JavaScript - returns Promise(Result)
pub fn get_user(
  client: squall.Client,
  id: String,
) -> Promise(Result(GetUserResponse, String)) {
  squall.execute_query(client, query, variables, decoder)
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

Squall is built with a modular, isomorphic architecture:

### Core Modules
- **`discovery`**: Finds and reads `.gql` files
- **`parser`**: Lexes and parses GraphQL operations
- **`schema`**: Introspects and parses GraphQL schemas
- **`type_mapping`**: Maps GraphQL types to Gleam types
- **`codegen`**: Generates platform-agnostic Gleam code
- **`error`**: Comprehensive error handling

### HTTP Adapter Pattern
- **`adapter`**: Defines the HTTP adapter interface
- **`adapter/erlang`**: Erlang implementation using `gleam_httpc`
- **`adapter/javascript`**: JavaScript implementation using Fetch API

The generated code calls `squall.execute_query()`, which automatically uses the correct HTTP adapter for your target platform.

## Development

### Running Tests

```bash
gleam test
```

### Project Structure

```
squall/
├── src/
│   ├── squall.gleam                    # CLI entry point & execute_query
│   └── squall/
│       ├── adapter.gleam               # HTTP adapter interface
│       ├── adapter/
│       │   ├── erlang.gleam           # Erlang HTTP adapter
│       │   └── javascript.gleam       # JavaScript HTTP adapter
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
├── birdie_snapshots/                   # Snapshot tests
└── examples/                           # Working examples
    ├── 01-erlang/                     # Erlang example
    ├── 02-javascript/                 # JavaScript example
    └── 03-isomorphic/                 # Cross-platform example
```

## Roadmap

### Completed ✅
- [x] Query support
- [x] Mutation support
- [x] Subscription support
- [x] Type-safe code generation
- [x] Schema introspection
- [x] Client abstraction with headers/authentication
- [x] **Isomorphic support** (Erlang + JavaScript targets)
- [x] HTTP adapter pattern
- [x] Target-conditional compilation

### In Progress 🚧
- [ ] Fragment support
- [ ] Directive handling (`@include`, `@skip`, etc.)
- [ ] Custom scalar type mapping
- [ ] Schema caching for faster regeneration

## Contributing

Contributions are welcome! Please:

1. Follow TDD principles
2. Add tests for new features
3. Update snapshots when changing code generation
4. Follow Gleam style guidelines

## License

Apache-2.0
