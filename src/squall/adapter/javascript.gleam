@target(javascript)
import gleam/fetch
@target(javascript)
import gleam/http/request
@target(javascript)
import gleam/http/response
@target(javascript)
import gleam/javascript/promise.{type Promise}
@target(javascript)
import gleam/result
@target(javascript)
import squall/adapter

@target(javascript)
/// JavaScript-specific HTTP adapter using the Fetch API via gleam_fetch.
/// This adapter works on both browser and Node.js JavaScript targets.
/// Returns a Promise that resolves to a Result.
pub fn adapter() -> adapter.HttpAdapter {
  fn(req: request.Request(String)) -> Promise(
    Result(response.Response(String), String),
  ) {
    fetch.send(req)
    |> promise.try_await(fetch.read_text_body)
    |> promise.map(result.map_error(_, fn(_) { "HTTP request failed" }))
  }
}
