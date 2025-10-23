import argv
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/io
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile
import squall/internal/codegen
import squall/internal/discovery
import squall/internal/error
import squall/internal/parser
import squall/internal/schema

pub fn main() {
  case argv.load().arguments {
    ["generate", endpoint] -> generate(endpoint)
    ["generate"] -> generate_with_env()
    _ -> {
      print_usage()
      Nil
    }
  }
}

fn print_usage() {
  io.println(
    "
Squall - Type-safe GraphQL client generator for Gleam

Usage:
  gleam run -m squall generate <endpoint>
  gleam run -m squall generate              # Uses GRAPHQL_ENDPOINT env var

Commands:
  generate <endpoint>   Generate Gleam code from .gql files

The tool will:
  1. Find all .gql files in src/**/graphql/ directories
  2. Introspect the GraphQL schema from the endpoint
  3. Generate type-safe Gleam functions for each query/mutation/subscription

Example:
  gleam run -m squall generate https://rickandmortyapi.com/graphql
",
  )
}

fn generate_with_env() {
  io.println("Error: GRAPHQL_ENDPOINT environment variable not set")
  io.println("Usage: gleam run -m squall generate <endpoint>")
  Nil
}

fn generate(endpoint: String) {
  io.println("🌊 Squall - GraphQL Code Generator")
  io.println("================================\n")

  io.println("📡 Introspecting GraphQL schema from: " <> endpoint)

  // Introspect schema
  case introspect_schema(endpoint) {
    Ok(schema_data) -> {
      io.println("✓ Schema introspected successfully\n")

      // Discover .gql files
      io.println("🔍 Discovering .gql files...")
      case discovery.find_graphql_files("src") {
        Ok(files) -> {
          io.println(
            "✓ Found " <> int_to_string(list.length(files)) <> " .gql file(s)\n",
          )

          // Process each file
          list.each(files, fn(file) {
            io.println("📝 Processing: " <> file.path)

            case parser.parse(file.content) {
              Ok(operation) -> {
                case
                  codegen.generate_operation(
                    file.operation_name,
                    operation,
                    schema_data,
                    endpoint,
                  )
                {
                  Ok(code) -> {
                    // Write generated code
                    let output_path =
                      string.replace(file.path, ".gql", ".gleam")

                    case simplifile.write(output_path, code) {
                      Ok(_) -> {
                        io.println("  ✓ Generated: " <> output_path)
                      }
                      Error(_) -> {
                        io.println("  ✗ Failed to write: " <> output_path)
                      }
                    }
                  }
                  Error(err) -> {
                    io.println(
                      "  ✗ Code generation failed: " <> error.to_string(err),
                    )
                  }
                }
              }
              Error(err) -> {
                io.println("  ✗ Parse failed: " <> error.to_string(err))
              }
            }
          })

          io.println("\n✨ Code generation complete!")
          Nil
        }
        Error(err) -> {
          io.println("✗ Failed to discover files: " <> error.to_string(err))
          Nil
        }
      }
    }
    Error(err) -> {
      io.println("✗ Schema introspection failed: " <> error.to_string(err))
      Nil
    }
  }
}

fn introspect_schema(endpoint: String) -> Result(schema.Schema, error.Error) {
  // Build introspection query
  let introspection_query =
    "
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          name
          kind
          description
          fields {
            name
            description
            type {
              ...TypeRef
            }
            args {
              name
              type {
                ...TypeRef
              }
            }
          }
          inputFields {
            name
            type {
              ...TypeRef
            }
          }
          enumValues {
            name
          }
          possibleTypes {
            name
          }
        }
      }
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    }
  "

  // Make HTTP request
  use response <- result.try(
    make_graphql_request(endpoint, introspection_query, "")
    |> result.map_error(fn(err) { error.HttpRequestFailed(err) }),
  )

  // Parse schema
  schema.parse_introspection_response(response)
}

fn make_graphql_request(
  endpoint: String,
  query: String,
  variables: String,
) -> Result(String, String) {
  // Build JSON body
  let vars_value = case variables {
    "" -> json.object([])
    _ -> json.string(variables)
  }

  let body =
    json.object([#("query", json.string(query)), #("variables", vars_value)])
    |> json.to_string

  // Create HTTP request
  use req <- result.try(
    request.to(endpoint)
    |> result.map_error(fn(_) { "Invalid endpoint URL: " <> endpoint }),
  )

  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_body(body)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("accept", "application/json")

  // Send request
  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { "Failed to send HTTP request to " <> endpoint }),
  )

  // Check status code
  case resp.status {
    200 -> Ok(resp.body)
    _ ->
      Error(
        "HTTP request failed with status "
        <> int_to_string(resp.status)
        <> ": "
        <> resp.body,
      )
  }
}

fn int_to_string(i: Int) -> String {
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    _ -> {
      // For larger numbers, convert via string representation
      let s = i
      case s >= 0 {
        True -> int_to_string(s / 10) <> int_to_string(s % 10)
        False -> "-" <> int_to_string(-s)
      }
    }
  }
}
