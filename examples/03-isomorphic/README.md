# Squall Isomorphic Example

This example demonstrates writing **cross-platform** Gleam code that works on **both Erlang and JavaScript** targets using Squall.

## What's Inside

- `isomorphic_example.gleam` - Code that runs on both targets
- `src/graphql/*.gql` - GraphQL query definitions
- `src/graphql/*.gleam` - Generated type-safe Gleam code

## The Magic: Target-Conditional Code

The same codebase works on both Erlang and JavaScript using `@target()` attributes:

```gleam
import squall
import graphql/multi_query

@target(javascript)
import gleam/javascript/promise

// Shared business logic - works on both targets!
fn handle_response(response: multi_query.MultiQueryResponse) -> Nil {
  io.println("✓ Success!")
  io.println(string.inspect(response))
}

fn handle_error(err: String) -> Nil {
  io.println("✗ Error: " <> err)
}

// Erlang implementation - synchronous
@target(erlang)
fn run() -> Nil {
  let client = squall.new_erlang_client("https://rickandmortyapi.com/graphql", [])
  let result = multi_query.multi_query(client)

  case result {
    Ok(response) -> handle_response(response)
    Error(err) -> handle_error(err)
  }
}

// JavaScript implementation - asynchronous with Promises
@target(javascript)
fn run() -> promise.Promise(Nil) {
  let client = squall.new_javascript_client("https://rickandmortyapi.com/graphql", [])

  multi_query.multi_query(client)
  |> promise.await(fn(result) {
    case result {
      Ok(response) -> {
        handle_response(response)
        promise.resolve(Nil)
      }
      Error(err) -> {
        handle_error(err)
        promise.resolve(Nil)
      }
    }
  })
}

pub fn main() {
  run()  // Works on both targets!
}
```

## Running on Erlang

```bash
# Build and run on Erlang
gleam run -m isomorphic_example
```

Output: `"Running on Erlang target (using gleam_httpc)"`

## Running on JavaScript

```bash
# Build and run on JavaScript (Node.js)
gleam run --target javascript -m isomorphic_example
```

Output: `"Running on JavaScript target (using Fetch API)"`

## How It Works

1. **Compile time**: Gleam includes only the code for the target platform
   - On Erlang: Only the `@target(erlang)` functions are compiled
   - On JavaScript: Only the `@target(javascript)` functions are compiled

2. **Runtime**: The appropriate HTTP adapter is used automatically
   - Erlang → `gleam_httpc` (synchronous, returns `Result`)
   - JavaScript → Fetch API (asynchronous, returns `Promise(Result)`)

3. **Shared business logic**: Response/error handlers work on both targets
   - `handle_response()` and `handle_error()` are platform-agnostic
   - Only the HTTP execution layer differs between platforms

4. **Generated code**: Completely platform-agnostic!
   - The same GraphQL code works everywhere
   - No platform-specific imports in generated files
   - Automatically adapts to sync (Erlang) or async (JavaScript) execution

## Build Artifacts

### Erlang Target
```bash
gleam build
# Outputs to: build/dev/erlang/
```

### JavaScript Target
```bash
gleam build --target javascript
# Outputs to: build/dev/javascript/
```

## Use Cases

This pattern is perfect for:

✅ **Full-stack Gleam apps** - Share GraphQL clients between backend (Erlang) and frontend (JavaScript)
✅ **Multi-platform libraries** - Write once, run everywhere
✅ **Migration scenarios** - Gradually move from one target to another
✅ **Testing** - Test the same business logic on different runtimes

## Features Demonstrated

✅ **Cross-Platform Code** - Single codebase for both targets
✅ **Conditional Compilation** - `@target()` attributes
✅ **HTTP Adapter Pattern** - Platform-specific implementations
✅ **Type Safety** - Fully typed across all targets
✅ **Zero Duplication** - Generated code shared between targets
