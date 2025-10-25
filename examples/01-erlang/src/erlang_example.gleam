import gleam/io
import gleam/json
import gleam/string
import graphql/multi_query
import squall

pub fn main() {
  io.println("Squall Multi-Field Query Example (Erlang)")
  io.println("==========================================\n")

  // Create an Erlang client (uses gleam_httpc for HTTP requests)
  let client = squall.new_erlang_client("https://rickandmortyapi.com/graphql", [])

  let result = multi_query.multi_query(client)

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
      let json_response = multi_query.multi_query_response_to_json(response)
      io.println(json.to_string(json_response))

      Nil
    }
    Error(err) -> {
      io.println("✗ Error: " <> err)
      Nil
    }
  }
}
