import gleam/io
import graphql/multi_query

pub fn main() {
  io.println("Squall Multi-Field Query Example")
  io.println("=================================\n")

  let result =
    multi_query.multi_query("https://rickandmortyapi.com/graphql")

  case result {
    Ok(response) -> {
      io.println("Response:")
      io.debug(response)
      Nil
    }
    Error(err) -> {
      io.println("Error: " <> err)
      Nil
    }
  }
}
