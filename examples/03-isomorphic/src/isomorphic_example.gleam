import gleam/io
import gleam/json
import gleam/string
import graphql/multi_query
import squall

@target(javascript)
import gleam/javascript/promise

// This example demonstrates how to write isomorphic code
// that works on both Erlang and JavaScript targets

pub fn main() {
  io.println("Squall Isomorphic Example")
  io.println("=========================\n")

  // Use target-specific main implementations
  run()
}

// Shared function to handle successful responses
fn handle_response(response: multi_query.MultiQueryResponse) -> Nil {
  io.println("✓ Success! Received response from API\n")

  // Print the Gleam data structure
  io.println("Gleam Response Structure:")
  io.println("-------------------------")
  io.println(string.inspect(response))

  // Convert to JSON and print
  io.println("\nJSON Response:")
  io.println("--------------")
  let json_response = multi_query.multi_query_response_to_json(response)
  io.println(json.to_string(json_response))
}

// Shared function to handle errors
fn handle_error(err: String) -> Nil {
  io.println("✗ Error: " <> err)
}

// Erlang implementation - synchronous
@target(erlang)
fn run() -> Nil {
  io.println("Running on Erlang target (using gleam_httpc)\n")

  let client = squall.new_erlang_client("https://rickandmortyapi.com/graphql", [])
  let result = multi_query.multi_query(client)

  case result {
    Ok(response) -> handle_response(response)
    Error(err) -> handle_error(err)
  }
}

// JavaScript implementation - asynchronous
@target(javascript)
fn run() -> promise.Promise(Nil) {
  io.println("Running on JavaScript target (using Fetch API)\n")

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
