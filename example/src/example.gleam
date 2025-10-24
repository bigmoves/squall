import gleam/io
import gleam/string
import graphql/multi_query
import squall

pub fn main() {
  io.println("Squall Multi-Field Query Example")
  io.println("=================================\n")

  let client = squall.new_client("https://rickandmortyapi.com/graphql", [])

  let result = multi_query.multi_query(client)

  case result {
    Ok(response) -> {
      io.println("Success! Received response from API")
      io.println("Response data: " <> string.inspect(response))
      Nil
    }
    Error(err) -> {
      io.println("Error: " <> err)
      Nil
    }
  }
}
