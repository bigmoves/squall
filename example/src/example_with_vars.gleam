import gleam/io
import graphql/multi_query_with_vars

pub fn main() {
  io.println("Squall Multi-Field Query Example (with Variables)")
  io.println("==================================================\n")

  io.println("Calling multi_query_with_vars with:")
  io.println("  page: 2")
  io.println("  name: \"rick\"")
  io.println("  locationId: \"1\"")
  io.println("  episodeIds: [1, 2]\n")

  let result =
    multi_query_with_vars.multi_query_with_vars(
      "https://rickandmortyapi.com/graphql",
      2,
      "rick",
      "1",
      [1, 2],
    )

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
