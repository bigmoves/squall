import gleam/io
import gleam/javascript/promise
import gleam/json
import gleam/string
import graphql/multi_query
import squall

pub fn main() {
  io.println("Squall Multi-Field Query Example (JavaScript/Node.js)")
  io.println("======================================================\n")

  // Create a JavaScript client (uses Fetch API for HTTP requests)
  let client =
    squall.new_javascript_client("https://rickandmortyapi.com/graphql", [])

  // On JavaScript, multi_query returns a Promise that resolves to a Result
  // We use promise.await to wait for the result
  multi_query.multi_query(client)
  |> promise.await(fn(result) {
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

        promise.resolve(Nil)
      }
      Error(err) -> {
        io.println("✗ Error: " <> err)
        promise.resolve(Nil)
      }
    }
  })
}
