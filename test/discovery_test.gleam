import gleam/list
import gleam/string
import gleeunit/should
import squall/internal/discovery

// Test: Discover .gql files in a directory
pub fn discover_graphql_files_test() {
  let result = discovery.find_graphql_files("test/fixtures")

  should.be_ok(result)
  let assert Ok(files) = result

  // Should find at least 2 files
  let count = list.length(files)
  should.be_true(count >= 2)

  // All files should end with .gql
  files
  |> list.all(fn(file) { string.ends_with(file.path, ".gql") })
  |> should.be_true()
}

// Test: Extract operation name from file path
pub fn extract_operation_name_test() {
  let result =
    discovery.extract_operation_name("test/fixtures/graphql/get_character.gql")

  should.be_ok(result)
  should.equal(result, Ok("get_character"))
}

// Test: Extract operation name from nested path
pub fn extract_operation_name_nested_test() {
  let result =
    discovery.extract_operation_name(
      "test/fixtures/nested/graphql/list_characters.gql",
    )

  should.be_ok(result)
  should.equal(result, Ok("list_characters"))
}

// Test: Invalid operation name (starts with number)
pub fn invalid_operation_name_test() {
  let result =
    discovery.extract_operation_name("test/fixtures/graphql/123invalid.gql")

  should.be_error(result)
}

// Test: Invalid operation name (contains hyphens)
pub fn invalid_operation_name_hyphens_test() {
  let result =
    discovery.extract_operation_name("test/fixtures/graphql/get-character.gql")

  should.be_error(result)
}

// Test: Valid operation name with underscores
pub fn valid_operation_name_underscores_test() {
  let result =
    discovery.extract_operation_name(
      "test/fixtures/graphql/get_character_by_id.gql",
    )

  should.be_ok(result)
  should.equal(result, Ok("get_character_by_id"))
}

// Test: Discover files returns sorted results
pub fn discover_files_sorted_test() {
  let result = discovery.find_graphql_files("test/fixtures")

  should.be_ok(result)
  let assert Ok(files) = result

  // Get file paths
  let paths = list.map(files, fn(f) { f.path })

  // Should be sorted alphabetically
  let sorted_paths = list.sort(paths, string.compare)
  should.equal(paths, sorted_paths)
}

// Test: Read file content
pub fn read_file_content_test() {
  let result = discovery.find_graphql_files("test/fixtures")

  should.be_ok(result)
  let assert Ok(files) = result

  // Find the get_character file
  let get_char_file =
    files
    |> list.find(fn(f) { string.contains(f.path, "get_character.gql") })

  should.be_ok(get_char_file)
  let assert Ok(file) = get_char_file

  // Content should contain the query keyword
  string.contains(file.content, "query")
  |> should.be_true()

  // Content should contain GetCharacter
  string.contains(file.content, "GetCharacter")
  |> should.be_true()
}
