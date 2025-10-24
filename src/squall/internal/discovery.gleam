import filepath
import gleam/list
import gleam/result
import gleam/string
import simplifile
import squall/internal/error.{type Error}

pub type GraphQLFile {
  GraphQLFile(path: String, operation_name: String, content: String)
}

/// Find all .gql files in graphql/ directories under the given root path
pub fn find_graphql_files(root: String) -> Result(List(GraphQLFile), Error) {
  case walk_directory(root) {
    Ok(paths) -> {
      // Filter for .gql files in graphql/ directories
      let gql_files =
        paths
        |> list.filter(fn(path) {
          string.ends_with(path, ".gql") && string.contains(path, "/graphql/")
        })
        |> list.sort(string.compare)

      // Read each file and create GraphQLFile records
      gql_files
      |> list.try_map(read_graphql_file)
    }
    Error(err) -> Error(error.CannotReadFile(root, simplifile_error(err)))
  }
}

/// Read a GraphQL file and extract its operation name
fn read_graphql_file(path: String) -> Result(GraphQLFile, Error) {
  use operation_name <- result.try(extract_operation_name(path))
  use content <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(err) {
      error.CannotReadFile(path, simplifile_error(err))
    }),
  )

  Ok(GraphQLFile(path, operation_name, content))
}

/// Extract operation name from file path (filename without .gql extension)
pub fn extract_operation_name(path: String) -> Result(String, Error) {
  // Get filename from path
  let filename = filepath.base_name(path)

  // Remove .gql extension
  let name = case string.ends_with(filename, ".gql") {
    True -> string.drop_end(filename, 4)
    False -> filename
  }

  // Validate it's a valid Gleam identifier
  case is_valid_gleam_identifier(name) {
    True -> Ok(name)
    False ->
      Error(error.InvalidOperationName(
        path,
        name,
        "Must start with lowercase letter and contain only letters, numbers, and underscores",
      ))
  }
}

/// Check if a string is a valid Gleam identifier (lowercase start, alphanumeric + underscore)
fn is_valid_gleam_identifier(name: String) -> Bool {
  case string.to_graphemes(name) {
    [] -> False
    [first, ..rest] -> {
      // First character must be lowercase letter
      let first_valid = is_lowercase_letter(first)

      // Rest must be alphanumeric or underscore
      let rest_valid =
        rest
        |> list.all(fn(char) {
          is_lowercase_letter(char) || is_digit(char) || char == "_"
        })

      first_valid && rest_valid
    }
  }
}

fn is_lowercase_letter(char: String) -> Bool {
  case char {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    _ -> False
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Recursively walk a directory and return all file paths
fn walk_directory(path: String) -> Result(List(String), simplifile.FileError) {
  use is_dir <- result.try(simplifile.is_directory(path))

  case is_dir {
    False -> Ok([path])
    True -> {
      use entries <- result.try(simplifile.read_directory(path))

      entries
      |> list.try_map(fn(entry) {
        let full_path = filepath.join(path, entry)
        walk_directory(full_path)
      })
      |> result.map(list.flatten)
    }
  }
}

fn simplifile_error(err: simplifile.FileError) -> String {
  case err {
    simplifile.Enoent -> "File or directory not found"
    simplifile.Eacces -> "Permission denied"
    simplifile.Epipe -> "Broken pipe"
    simplifile.Eexist -> "File already exists"
    simplifile.Enotdir -> "Not a directory"
    simplifile.Eisdir -> "Is a directory"
    simplifile.Unknown(msg) -> "Unknown error: " <> msg
    _ -> "File system error"
  }
}
