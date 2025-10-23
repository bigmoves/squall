import gleam/json

pub type Error {
  // HTTP/Network errors
  HttpRequestFailed(reason: String)
  HttpInvalidResponse(status: Int, body: String)

  // Schema introspection errors
  SchemaIntrospectionFailed(reason: String)
  InvalidSchemaResponse(reason: String)

  // File I/O errors
  CannotReadFile(file: String, reason: String)
  CannotWriteToFile(file: String, reason: String)

  // Query parsing errors
  InvalidGraphQLSyntax(file: String, line: Int, message: String)
  InvalidOperationName(file: String, name: String, reason: String)

  // Type errors
  UnsupportedGraphQLType(file: String, type_name: String)
  InvalidTypeMapping(reason: String)

  // Code generation errors
  CodeGenerationFailed(reason: String)
}

pub fn to_string(error: Error) -> String {
  case error {
    HttpRequestFailed(reason) -> "HTTP request failed: " <> reason
    HttpInvalidResponse(status, body) ->
      "Invalid HTTP response (status " <> int_to_string(status) <> "): " <> body
    SchemaIntrospectionFailed(reason) ->
      "Schema introspection failed: " <> reason
    InvalidSchemaResponse(reason) -> "Invalid schema response: " <> reason
    CannotReadFile(file, reason) -> "Cannot read file " <> file <> ": " <> reason
    CannotWriteToFile(file, reason) ->
      "Cannot write to file " <> file <> ": " <> reason
    InvalidGraphQLSyntax(file, line, message) ->
      "Invalid GraphQL syntax in "
      <> file
      <> " at line "
      <> int_to_string(line)
      <> ": "
      <> message
    InvalidOperationName(file, name, reason) ->
      "Invalid operation name '"
      <> name
      <> "' in "
      <> file
      <> ": "
      <> reason
    UnsupportedGraphQLType(file, type_name) ->
      "Unsupported GraphQL type '" <> type_name <> "' in " <> file
    InvalidTypeMapping(reason) -> "Invalid type mapping: " <> reason
    CodeGenerationFailed(reason) -> "Code generation failed: " <> reason
  }
}

fn int_to_string(i: Int) -> String {
  case i {
    _ -> {
      // Use string builder for int to string
      let assert Ok(s) = json.to_string(json.int(i)) |> Ok
      s
    }
  }
}
