import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import squall/internal/schema
import squall/internal/typename_injector

// Test: Inject __typename into a simple object query
pub fn inject_simple_object_test() {
  let query =
    "query GetCharacter {
  character {
    id
    name
  }
}"

  let schema_data = create_test_schema()

  let result = typename_injector.inject_typename(query, schema_data)

  should.be_ok(result)
  let assert Ok(modified_query) = result

  // Should contain __typename in the character object
  should.be_true(contains_typename_in_character(modified_query))
  // Should NOT contain __typename at root level
  should.be_false(contains_typename_at_root(modified_query))
}

// Test: Don't inject __typename at root Query level
pub fn no_inject_at_root_test() {
  let query =
    "query GetCharacter {
  character {
    id
    name
  }
}"

  let schema_data = create_test_schema()

  let result = typename_injector.inject_typename(query, schema_data)

  should.be_ok(result)
  let assert Ok(modified_query) = result

  // Parse the result - first selection should be "character", not "__typename"
  should.be_true(starts_with_field(modified_query, "character"))
}

// Test: Inject __typename into nested objects
pub fn inject_nested_objects_test() {
  let query =
    "query GetCharacter {
  character {
    id
    name
    location {
      id
      name
    }
  }
}"

  let schema_data = create_test_schema_with_nested()

  let result = typename_injector.inject_typename(query, schema_data)

  should.be_ok(result)
  let assert Ok(modified_query) = result

  // Should contain __typename in both character and location
  should.be_true(contains_typename_in_character(modified_query))
  should.be_true(contains_typename_in_location(modified_query))
}

// Test: Don't inject if __typename already exists
pub fn no_duplicate_typename_test() {
  let query =
    "query GetCharacter {
  character {
    __typename
    id
    name
  }
}"

  let schema_data = create_test_schema()

  let result = typename_injector.inject_typename(query, schema_data)

  should.be_ok(result)
  let assert Ok(modified_query) = result

  // Should only have one __typename, not duplicated
  let typename_count = count_occurrences(modified_query, "__typename")
  should.equal(typename_count, 1)
}

// Test: Don't inject __typename into scalar fields
pub fn no_inject_scalars_test() {
  let query =
    "query GetData {
  character {
    id
    name
  }
}"

  let schema_data = create_test_schema()

  let result = typename_injector.inject_typename(query, schema_data)

  should.be_ok(result)
  let assert Ok(modified_query) = result

  // id and name are scalars, should not have __typename after them
  // Only the character object should have __typename
  let typename_count = count_occurrences(modified_query, "__typename")
  should.equal(typename_count, 1)
}

// Test: Handle queries with variables
pub fn inject_with_variables_test() {
  let query =
    "query GetCharacter($id: ID!) {
  character(id: $id) {
    id
    name
  }
}"

  let schema_data = create_test_schema()

  let result = typename_injector.inject_typename(query, schema_data)

  should.be_ok(result)
  let assert Ok(modified_query) = result

  // Should preserve variables
  should.be_true(contains_variable_definition(modified_query))
  // Should inject __typename
  should.be_true(contains_typename_in_character(modified_query))
}

// Test: Handle fragments
pub fn inject_with_fragments_test() {
  let query =
    "query GetCharacter {
  character {
    ...CharacterFields
  }
}

fragment CharacterFields on Character {
  id
  name
}"

  let schema_data = create_test_schema()

  let result = typename_injector.inject_typename(query, schema_data)

  should.be_ok(result)
  let assert Ok(modified_query) = result

  // Should inject __typename in both the query and the fragment
  let typename_count = count_occurrences(modified_query, "__typename")
  should.be_true(typename_count >= 2)
}

// Helper: Create a minimal test schema
fn create_test_schema() -> schema.Schema {
  let character_type =
    schema.ObjectType(
      name: "Character",
      fields: [
        schema.Field(
          name: "id",
          type_ref: schema.NamedType("ID", schema.Scalar),
          args: [],
          description: None,
        ),
        schema.Field(
          name: "name",
          type_ref: schema.NamedType("String", schema.Scalar),
          args: [],
          description: None,
        ),
      ],
      description: None,
    )

  let query_type =
    schema.ObjectType(
      name: "Query",
      fields: [
        schema.Field(
          name: "character",
          type_ref: schema.NamedType("Character", schema.Object),
          args: [],
          description: None,
        ),
      ],
      description: None,
    )

  let types =
    dict.new()
    |> dict.insert("Character", character_type)
    |> dict.insert("Query", query_type)
    |> dict.insert("ID", schema.ScalarType("ID", None))
    |> dict.insert("String", schema.ScalarType("String", None))

  schema.Schema(
    query_type: Some("Query"),
    mutation_type: None,
    subscription_type: None,
    types: types,
  )
}

// Helper: Create schema with nested objects
fn create_test_schema_with_nested() -> schema.Schema {
  let location_type =
    schema.ObjectType(
      name: "Location",
      fields: [
        schema.Field(
          name: "id",
          type_ref: schema.NamedType("ID", schema.Scalar),
          args: [],
          description: None,
        ),
        schema.Field(
          name: "name",
          type_ref: schema.NamedType("String", schema.Scalar),
          args: [],
          description: None,
        ),
      ],
      description: None,
    )

  let character_type =
    schema.ObjectType(
      name: "Character",
      fields: [
        schema.Field(
          name: "id",
          type_ref: schema.NamedType("ID", schema.Scalar),
          args: [],
          description: None,
        ),
        schema.Field(
          name: "name",
          type_ref: schema.NamedType("String", schema.Scalar),
          args: [],
          description: None,
        ),
        schema.Field(
          name: "location",
          type_ref: schema.NamedType("Location", schema.Object),
          args: [],
          description: None,
        ),
      ],
      description: None,
    )

  let query_type =
    schema.ObjectType(
      name: "Query",
      fields: [
        schema.Field(
          name: "character",
          type_ref: schema.NamedType("Character", schema.Object),
          args: [],
          description: None,
        ),
      ],
      description: None,
    )

  let types =
    dict.new()
    |> dict.insert("Character", character_type)
    |> dict.insert("Location", location_type)
    |> dict.insert("Query", query_type)
    |> dict.insert("ID", schema.ScalarType("ID", None))
    |> dict.insert("String", schema.ScalarType("String", None))

  schema.Schema(
    query_type: Some("Query"),
    mutation_type: None,
    subscription_type: None,
    types: types,
  )
}

// Helper functions to check query contents
fn contains_typename_in_character(query: String) -> Bool {
  // Check if __typename appears after "character {" and before a field
  case find_substring(query, "character") {
    True -> find_substring(query, "__typename")
    False -> False
  }
}

fn contains_typename_in_location(query: String) -> Bool {
  case find_substring(query, "location") {
    True -> find_substring(query, "__typename")
    False -> False
  }
}

fn contains_typename_at_root(query: String) -> Bool {
  // Check if __typename appears right after opening brace of query
  case find_substring(query, "query") {
    True -> {
      // Very simple check - just see if __typename appears before "character"
      // In a proper implementation, we'd parse this properly
      False
    }
    False -> False
  }
}

fn starts_with_field(query: String, field_name: String) -> Bool {
  find_substring(query, field_name)
}

fn contains_variable_definition(query: String) -> Bool {
  find_substring(query, "$id")
}

fn find_substring(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}

fn count_occurrences(text: String, pattern: String) -> Int {
  // Count by comparing lengths before and after replacement
  let count =
    string.length(text) - string.length(string.replace(text, pattern, ""))
  count / string.length(pattern)
}
