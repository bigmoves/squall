# Squall

A type-safe **sans-io** GraphQL client generator for Gleam.

![Squall](https://images.unsplash.com/photo-1698115989309-16f9d34c04e9?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=2340)

> This project is in early development and may contain bugs/unsupported GraphQL queries.

## What is Sans-IO?

Squall generates functions to **build HTTP requests** and **parse HTTP responses**, but doesn't send them. You control the HTTP layer.

**Benefits:**
- Can be used on both erlang and javascript runtimes
- Use any HTTP client you want
- Easy to test without mocking
- Full control over retries, timeouts, logging

## Installation

```bash
gleam add squall
```

## Quick Start

1. **Create a `.gql` file** at `src/my_app/graphql/get_user.gql`:

```graphql
query GetUser($id: ID!) {
  user(id: $id) {
    id
    name
    email
  }
}
```

2. **Generate code:**

```bash
gleam run -m squall generate https://api.example.com/graphql
```

3. **Use it:**

```gleam
import squall
import my_app/graphql/get_user
import gleam/httpc

pub fn main() {
  // 1. Create client (just config)
  let client = squall.new("https://api.example.com/graphql", [])

  // 2. Build request (no I/O)
  let assert Ok(request) = get_user.get_user(client, "123")

  // 3. Send request (you control this)
  let assert Ok(response) = httpc.send(request)

  // 4. Parse response (no I/O)
  let assert Ok(data) = get_user.parse_get_user_response(response.body)

  // 5. Use the data
  io.debug(data.user)
}
```

## How It Works

For each `.gql` file, Squall generates two functions:

```gleam
// Builds HTTP request - no I/O
pub fn get_user(client: Client, id: String) -> Result(Request(String), String)

// Parses response - no I/O
pub fn parse_get_user_response(body: String) -> Result(GetUserResponse, String)
```

You send the request with your own HTTP client.

## JavaScript Target

Same generated code works on JavaScript. Just use a different HTTP client:

```gleam
import gleam/fetch
import gleam/javascript/promise

let client = squall.new("https://api.example.com/graphql", [])
let assert Ok(request) = get_user.get_user(client, "123")

fetch.send(request)
|> promise.try_await(fetch.read_text_body)
|> promise.map(fn(result) {
  case result {
    Ok(response) -> {
      let assert Ok(data) = get_user.parse_get_user_response(response.body)
      io.debug(data.user)
    }
    Error(_) -> io.println("Request failed")
  }
})
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

```bash
# Generate code
gleam run -m squall generate <endpoint>

# Example
gleam run -m squall generate https://rickandmortyapi.com/graphql
```

## Examples

See the [example/](./example/) directory for a complete working example.

## Roadmap

- [ ] Directive handling (`@include`, `@skip`)
- [ ] Custom scalar type mapping
- [ ] Schema caching

## License

Apache-2.0
