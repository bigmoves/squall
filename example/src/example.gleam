import gleam/http/request
import gleam/io
import gleam/result

import squall

// Import the generated GraphQL code
import graphql/get_character

// For Erlang target (default)
import gleam/httpc

// For JavaScript target: comment out httpc above and uncomment these
// import gleam/javascript/promise
// import gleam/fetch

pub fn main() {
  let client = squall.new("https://rickandmortyapi.com/graphql", [])
  let assert Ok(request) = get_character.get_character(client, "1")
  let assert Ok(body) = send_erlang(request)
  io.println(body)
}

// ==================== ERLANG HTTP CLIENT ====================
fn send_erlang(request: request.Request(String)) -> Result(String, String) {
  httpc.send(request)
  |> result.map(fn(resp) { resp.body })
  |> result.map_error(fn(_) { "HTTP request failed" })
}
// ==================== JAVASCRIPT HTTP CLIENT ====================
// Uncomment this function when using JavaScript target
//
// fn send_javascript(
//   request: Request(String),
// ) -> promise.Promise(Result(String, String)) {
//   fetch.send(request)
//   |> promise.try_await(fetch.read_text_body)
//   |> promise.map(fn(result) {
//     result
//     |> result.map(fn(resp) { resp.body })
//     |> result.map_error(fn(_) { "HTTP request failed" })
//   })
// }
