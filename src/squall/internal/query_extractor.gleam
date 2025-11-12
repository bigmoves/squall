import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile
import squall/internal/graphql_ast

/// A GraphQL query definition extracted from source code
pub type QueryDefinition {
  QueryDefinition(name: String, query: String, file_path: String)
}

/// Scan for component files containing @squall-query markers
pub fn scan_component_files(root: String) -> Result(List(String), String) {
  scan_directory_recursive(root, [])
}

fn scan_directory_recursive(
  path: String,
  acc: List(String),
) -> Result(List(String), String) {
  case simplifile.read_directory(path) {
    Ok(entries) -> {
      list.fold(entries, Ok(acc), fn(result_acc, entry) {
        case result_acc {
          Error(e) -> Error(e)
          Ok(files) -> {
            let entry_path = path <> "/" <> entry
            case simplifile.is_file(entry_path) {
              Ok(True) ->
                case string.ends_with(entry, ".gleam") {
                  True -> Ok(list.append(files, [entry_path]))
                  False -> Ok(files)
                }
              _ ->
                case simplifile.is_directory(entry_path) {
                  Ok(True) -> scan_directory_recursive(entry_path, files)
                  _ -> Ok(files)
                }
            }
          }
        }
      })
    }
    Error(_) -> Error("Failed to read directory: " <> path)
  }
}

/// Extract query definitions from a Gleam source file
pub fn extract_from_file(
  file_path: String,
) -> Result(List(QueryDefinition), String) {
  case simplifile.read(file_path) {
    Ok(content) -> extract_from_content(content, file_path)
    Error(_) -> Error("Failed to read file: " <> file_path)
  }
}

/// Extract query definitions from file content
fn extract_from_content(
  content: String,
  file_path: String,
) -> Result(List(QueryDefinition), String) {
  let lines = string.split(content, "\n")
  let queries = find_queries_in_lines(lines, file_path, None, [])
  Ok(queries)
}

/// Recursively process lines to find GraphQL query blocks
fn find_queries_in_lines(
  lines: List(String),
  file_path: String,
  current_query: Option(List(String)),
  accumulated: List(QueryDefinition),
) -> List(QueryDefinition) {
  case lines {
    [] -> {
      // End of file - finalize any pending query
      case current_query {
        Some(query_lines) -> {
          let query = string.join(query_lines, "\n")
          case parse_operation_name(query) {
            Ok(name) ->
              list.append(accumulated, [
                QueryDefinition(name: name, query: query, file_path: file_path),
              ])
            Error(_) -> accumulated
          }
        }
        None -> accumulated
      }
    }
    [line, ..rest] -> {
      let trimmed = string.trim(line)

      case string.starts_with(trimmed, "/// ```graphql") {
        True -> {
          // Start collecting query lines
          find_queries_in_lines(rest, file_path, Some([]), accumulated)
        }
        False ->
          case string.starts_with(trimmed, "/// ```") {
            True ->
              case current_query {
                Some(query_lines) -> {
                  // End of GraphQL block - parse and extract name
                  let query = string.join(query_lines, "\n")
                  case parse_operation_name(query) {
                    Ok(name) -> {
                      let new_accumulated =
                        list.append(accumulated, [
                          QueryDefinition(
                            name: name,
                            query: query,
                            file_path: file_path,
                          ),
                        ])
                      find_queries_in_lines(
                        rest,
                        file_path,
                        None,
                        new_accumulated,
                      )
                    }
                    Error(_) ->
                      // Skip queries that can't be parsed or don't have names
                      find_queries_in_lines(rest, file_path, None, accumulated)
                  }
                }
                None ->
                  find_queries_in_lines(
                    rest,
                    file_path,
                    current_query,
                    accumulated,
                  )
              }
            False ->
              case current_query {
                Some(query_lines) -> {
                  // Inside GraphQL block - collect line
                  let clean_line = string.replace(trimmed, "/// ", "")
                  let updated_lines = list.append(query_lines, [clean_line])
                  find_queries_in_lines(
                    rest,
                    file_path,
                    Some(updated_lines),
                    accumulated,
                  )
                }
                None ->
                  find_queries_in_lines(
                    rest,
                    file_path,
                    current_query,
                    accumulated,
                  )
              }
          }
      }
    }
  }
}

/// Parse a GraphQL query and extract the operation name
fn parse_operation_name(query: String) -> Result(String, Nil) {
  use document <- result.try(
    graphql_ast.parse_document(query) |> result.replace_error(Nil),
  )
  use operation <- result.try(
    graphql_ast.get_main_operation(document) |> result.replace_error(Nil),
  )
  graphql_ast.get_operation_name(operation) |> option.to_result(Nil)
}
