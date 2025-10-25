import gleam/http/request.{type Request}
import gleam/http/response.{type Response}

@target(javascript)
import gleam/javascript/promise.{type Promise}

@target(erlang)
/// HTTP adapter function type for Erlang target.
/// Synchronously returns a Result with Response or error string.
pub type HttpAdapter =
  fn(Request(String)) -> Result(Response(String), String)

@target(javascript)
/// HTTP adapter function type for JavaScript target.
/// Asynchronously returns a Promise containing a Result with Response or error string.
pub type HttpAdapter =
  fn(Request(String)) -> Promise(Result(Response(String), String))
