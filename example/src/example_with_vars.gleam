import gleam/io
import gleam/json
import gleam/string
import graphql/multi_query_with_vars
import squall

pub fn main() {
  io.println("Squall Multi-Field Query Example (with Variables)")
  io.println("==================================================\n")

  io.println("Calling multi_query_with_vars with:")
  io.println("  page: 2")
  io.println("  name: \"rick\"")
  io.println("  locationId: \"1\"")
  io.println("  episodeIds: [1, 2]\n")

  let client = squall.new_client("https://rickandmortyapi.com/graphql", [])

  let result =
    multi_query_with_vars.multi_query_with_vars(client, 2, "rick", "1", [1, 2])

  case result {
    Ok(response) -> {
      io.println("✓ Success! Received response from API\n")

      // Print the Gleam data structure
      io.println("Gleam Response Structure:")
      io.println("-------------------------")
      io.println(string.inspect(response))

      // Convert to JSON and print
      io.println("\nJSON Response:")
      io.println("--------------")
      let json_response =
        multi_query_with_vars.multi_query_with_vars_response_to_json(response)
      io.println(json.to_string(json_response))

      Nil
    }
    Error(err) -> {
      io.println("✗ Error: " <> err)
      Nil
    }
  }
}
