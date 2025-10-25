@target(erlang)
import gleam/http/request.{type Request}
@target(erlang)
import gleam/http/response.{type Response}
@target(erlang)
import gleam/httpc
@target(erlang)
import gleam/result
@target(erlang)
import squall/adapter.{type HttpAdapter}

@target(erlang)
/// Erlang-specific HTTP adapter using gleam_httpc.
/// This adapter works on the Erlang/OTP target.
pub fn adapter() -> HttpAdapter {
  fn(req: Request(String)) -> Result(Response(String), String) {
    httpc.send(req)
    |> result.map_error(fn(_) { "HTTP request failed" })
  }
}
