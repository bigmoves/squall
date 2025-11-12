import gleam/dynamic/decode
import gleam/http
import gleam/http/request.{type Request}
import gleam/json
import gleam/list
import gleam/result
import gleam/string

@target(erlang)
import argv

@target(erlang)
import gleam/io

@target(erlang)
import gleam/httpc

@target(erlang)
import simplifile

@target(erlang)
import squall/internal/codegen

@target(erlang)
import squall/internal/discovery

@target(erlang)
import squall/internal/error

@target(erlang)
import squall/internal/graphql_ast

@target(erlang)
import squall/internal/schema

@target(erlang)
import squall/internal/query_extractor

@target(erlang)
import squall/internal/registry_codegen

@target(erlang)
import squall/internal/typename_injector

/// A GraphQL client with endpoint and headers configuration.
/// This client follows the sans-io pattern: it builds HTTP requests but doesn't send them.
/// You must use your own HTTP client to send the requests.
pub type Client {
  Client(endpoint: String, headers: List(#(String, String)))
}

/// Create a new GraphQL client with custom headers.
///
/// ## Example
///
/// ```gleam
/// let client = squall.new("https://api.example.com/graphql", [])
/// ```
pub fn new(endpoint: String, headers: List(#(String, String))) -> Client {
  Client(endpoint: endpoint, headers: headers)
}

/// Create a new GraphQL client with bearer token authentication.
///
/// ## Example
///
/// ```gleam
/// let client = squall.new_with_auth("https://api.example.com/graphql", "my-token")
/// ```
pub fn new_with_auth(endpoint: String, token: String) -> Client {
  Client(endpoint: endpoint, headers: [#("Authorization", "Bearer " <> token)])
}

/// Prepare an HTTP request for a GraphQL query.
/// This function builds the request but does not send it.
/// You must send the request using your own HTTP client.
///
/// ## Example
///
/// ```gleam
/// let client = squall.new("https://api.example.com/graphql", [])
/// let request = squall.prepare_request(
///   client,
///   "query { users { id name } }",
///   json.object([]),
/// )
///
/// // Send with your HTTP client (Erlang example)
/// let assert Ok(response) = httpc.send(request)
///
/// // Parse the response
/// let assert Ok(data) = squall.parse_response(response.body, your_decoder)
/// ```
pub fn prepare_request(
  client: Client,
  query: String,
  variables: json.Json,
) -> Result(Request(String), String) {
  let body =
    json.object([#("query", json.string(query)), #("variables", variables)])

  use req <- result.try(
    request.to(client.endpoint)
    |> result.map_error(fn(_) { "Invalid endpoint URL" }),
  )

  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_body(json.to_string(body))
    |> request.set_header("content-type", "application/json")

  let req =
    list.fold(client.headers, req, fn(r, header) {
      request.set_header(r, header.0, header.1)
    })

  Ok(req)
}

/// Parse a GraphQL response body using the provided decoder.
/// This function decodes the JSON response and extracts the data field.
///
/// ## Example
///
/// ```gleam
/// let decoder = decode.field("users", decode.list(user_decoder))
///
/// case squall.parse_response(response_body, decoder) {
///   Ok(users) -> io.println("Got users!")
///   Error(err) -> io.println("Parse error: " <> err)
/// }
/// ```
pub fn parse_response(
  body: String,
  decoder: decode.Decoder(a),
) -> Result(a, String) {
  use json_value <- result.try(
    json.parse(from: body, using: decode.dynamic)
    |> result.map_error(fn(_) { "Failed to decode JSON response" }),
  )

  let data_decoder = {
    use data <- decode.field("data", decoder)
    decode.success(data)
  }

  decode.run(json_value, data_decoder)
  |> result.map_error(fn(errors) {
    "Failed to decode response data: "
    <> string.inspect(errors)
    <> ". Response body: "
    <> body
  })
}

@target(erlang)
pub fn main() {
  case argv.load().arguments {
    ["generate", endpoint] -> generate(endpoint)
    ["generate"] -> generate_with_env()
    ["unstable-cache", endpoint] -> unstable_cache(endpoint)
    ["unstable-cache"] -> {
      io.println("Error: Endpoint required")
      io.println("Usage: gleam run -m squall unstable-cache <endpoint>")
      Nil
    }
    _ -> {
      print_usage()
      Nil
    }
  }
}

@target(erlang)
fn print_usage() {
  io.println(
    "
Squall - Type-safe GraphQL client generator for Gleam

Usage:
  gleam run -m squall generate <endpoint>
  gleam run -m squall unstable-cache <endpoint>

Commands:
  generate <endpoint>            Generate Gleam code from .gql files
  unstable-cache <endpoint>      Extract GraphQL queries from doc comments and generate types and cache registry

The generate command will:
  1. Find all .gql files in src/**/graphql/ directories
  2. Introspect the GraphQL schema from the endpoint
  3. Generate type-safe Gleam functions for each query/mutation/subscription

The unstable-cache command will:
  1. Scan all .gleam files in src/ for GraphQL query blocks in doc comments
  2. Introspect the GraphQL schema from the endpoint
  3. Automatically inject __typename into queries for cache normalization
  4. Generate type-safe code for each query at src/generated/queries/
  5. Generate a registry initialization module at src/generated/queries.gleam

Examples:
  gleam run -m squall generate https://rickandmortyapi.com/graphql
  gleam run -m squall unstable-cache https://rickandmortyapi.com/graphql
",
  )
}

@target(erlang)
fn generate_with_env() {
  io.println("Usage: gleam run -m squall generate <endpoint>")
  Nil
}

@target(erlang)
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

            case graphql_ast.parse_document(file.content) {
              Ok(document) -> {
                // Extract main operation and fragments
                case graphql_ast.get_main_operation(document) {
                  Ok(operation) -> {
                    let fragments =
                      graphql_ast.get_fragment_definitions(document)

                    case
                      codegen.generate_operation_with_fragments(
                        file.operation_name,
                        file.content,
                        operation,
                        fragments,
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

@target(erlang)
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

@target(erlang)
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

  // Send request using httpc (generator always runs on Erlang)
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

@target(erlang)
fn unstable_cache(endpoint: String) {
  let queries_output_dir = "src/generated/queries"
  let registry_output_path = "src/generated/queries.gleam"

  io.println("🌊 Squall")
  io.println("============================================\n")

  io.println("🔍 Scanning for GraphQL queries in src/...")

  // Scan for component files
  case query_extractor.scan_component_files("src") {
    Ok(files) -> {
      io.println(
        "✓ Found " <> int_to_string(list.length(files)) <> " .gleam file(s)\n",
      )

      // Extract queries from each file
      io.println("📝 Extracting queries...")
      let all_queries =
        list.fold(files, [], fn(acc, file_path) {
          case query_extractor.extract_from_file(file_path) {
            Ok(queries) -> {
              list.each(queries, fn(q) {
                io.println("  ✓ Found: " <> q.name <> " in " <> file_path)
              })
              list.append(acc, queries)
            }
            Error(err) -> {
              io.println(
                "  ✗ Failed to extract from " <> file_path <> ": " <> err,
              )
              acc
            }
          }
        })

      case list.length(all_queries) {
        0 -> {
          io.println("\n⚠ No GraphQL queries found")
          io.println(
            "Add GraphQL query blocks to your doc comments with named operations",
          )
          Nil
        }
        _ -> {
          io.println(
            "\n✓ Extracted "
            <> int_to_string(list.length(all_queries))
            <> " quer"
            <> case list.length(all_queries) {
              1 -> "y"
              _ -> "ies"
            },
          )

          // Introspect schema
          io.println("\n📡 Introspecting GraphQL schema from: " <> endpoint)
          case introspect_schema(endpoint) {
            Ok(schema_data) -> {
              io.println("✓ Schema introspected successfully\n")

              // Generate type-safe code for each query
              io.println("🔧 Generating type-safe code...")
              list.each(all_queries, fn(query_def) {
                io.println("  • " <> query_def.name)

                // Parse the GraphQL query
                case graphql_ast.parse_document(query_def.query) {
                  Ok(document) -> {
                    case graphql_ast.get_main_operation(document) {
                      Ok(operation) -> {
                        let fragments =
                          graphql_ast.get_fragment_definitions(document)

                        // Generate code - convert query name to snake_case for module name
                        let module_name = to_snake_case(query_def.name)

                        case
                          codegen.generate_operation_with_fragments(
                            module_name,
                            query_def.query,
                            operation,
                            fragments,
                            schema_data,
                            endpoint,
                          )
                        {
                          Ok(code) -> {
                            let file_name = module_name
                            let module_path =
                              queries_output_dir <> "/" <> file_name <> ".gleam"

                            // Create directory if needed
                            let _ =
                              simplifile.create_directory_all(
                                queries_output_dir,
                              )

                            case simplifile.write(module_path, code) {
                              Ok(_) -> io.println("    ✓ " <> module_path)
                              Error(_) ->
                                io.println(
                                  "    ✗ Failed to write " <> module_path,
                                )
                            }
                          }
                          Error(err) -> {
                            io.println(
                              "    ✗ Codegen failed: " <> error.to_string(err),
                            )
                          }
                        }
                      }
                      Error(err) -> {
                        io.println(
                          "    ✗ Parse failed: " <> error.to_string(err),
                        )
                      }
                    }
                  }
                  Error(err) -> {
                    io.println("    ✗ Parse failed: " <> error.to_string(err))
                  }
                }
              })

              // Generate registry code with __typename injected
              io.println("\n📦 Generating registry module...")
              // Inject __typename into all query strings for the registry
              let queries_with_typename =
                list.map(all_queries, fn(query_def) {
                  case
                    typename_injector.inject_typename(
                      query_def.query,
                      schema_data,
                    )
                  {
                    Ok(injected_query) ->
                      query_extractor.QueryDefinition(
                        name: query_def.name,
                        query: injected_query,
                        file_path: query_def.file_path,
                      )
                    Error(_) -> query_def
                  }
                })
              let code =
                registry_codegen.generate_registry_module(queries_with_typename)

              // Write to output file
              case simplifile.write(registry_output_path, code) {
                Ok(_) -> {
                  io.println("✓ Generated: " <> registry_output_path)
                  io.println("\n✨ Code generation complete!")
                  Nil
                }
                Error(_) -> {
                  io.println("✗ Failed to write: " <> registry_output_path)
                  Nil
                }
              }
            }
            Error(err) -> {
              io.println(
                "✗ Schema introspection failed: " <> error.to_string(err),
              )
              Nil
            }
          }
        }
      }
    }
    Error(err) -> {
      io.println("✗ Failed to scan files: " <> err)
      Nil
    }
  }
}

@target(erlang)
fn to_snake_case(s: String) -> String {
  // Simple conversion: GetCharacters -> get_characters
  s
  |> string.to_graphemes
  |> list.index_fold([], fn(acc, char, index) {
    case is_uppercase(char) {
      True ->
        case index {
          0 -> list.append(acc, [string.lowercase(char)])
          _ -> list.append(acc, ["_", string.lowercase(char)])
        }
      False -> list.append(acc, [char])
    }
  })
  |> string.join("")
}

@target(erlang)
fn is_uppercase(s: String) -> Bool {
  s == string.uppercase(s) && s != string.lowercase(s)
}

@target(erlang)
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
