import birdie
import gleam/dict
import gleam/option.{None, Some}
import squall/internal/codegen
import squall/internal/parser
import squall/internal/schema

// Test: Generate simple query function
pub fn generate_simple_query_test() {
  let query_source =
    "
    query GetUser {
      user {
        id
        name
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  // Create mock schema with user type
  let user_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field("name", schema.NamedType("String", schema.Scalar), [], None),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      None,
      None,
      dict.from_list([
        #("User", schema.ObjectType("User", user_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "user",
                schema.NamedType("User", schema.Object),
                [],
                None,
              ),
            ],
            None,
          ),
        ),
      ]),
    )

  let result =
    codegen.generate_operation("get_user", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Simple query function generation")
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with variables
pub fn generate_query_with_variables_test() {
  let query_source =
    "
    query GetUser($id: ID!) {
      user(id: $id) {
        id
        name
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  let user_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field("name", schema.NamedType("String", schema.Scalar), [], None),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      None,
      None,
      dict.from_list([
        #("User", schema.ObjectType("User", user_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "user",
                schema.NamedType("User", schema.Object),
                [
                  schema.InputValue(
                    "id",
                    schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
                    None,
                  ),
                ],
                None,
              ),
            ],
            None,
          ),
        ),
      ]),
    )

  let result =
    codegen.generate_operation("get_user", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Query with variables function generation")
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with nested types
pub fn generate_query_with_nested_types_test() {
  let query_source =
    "
    query GetCharacter($id: ID!) {
      character(id: $id) {
        id
        name
        status
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  let character_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "name",
      schema.NonNullType(schema.NamedType("String", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "status",
      schema.NonNullType(schema.NamedType("String", schema.Scalar)),
      [],
      None,
    ),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      None,
      None,
      dict.from_list([
        #("Character", schema.ObjectType("Character", character_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "character",
                schema.NamedType("Character", schema.Object),
                [
                  schema.InputValue(
                    "id",
                    schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
                    None,
                  ),
                ],
                None,
              ),
            ],
            None,
          ),
        ),
      ]),
    )

  let result =
    codegen.generate_operation("get_character", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Query with nested types generation")
    }
    Error(_) -> Nil
  }
}

// Test: Generate mutation function
pub fn generate_mutation_test() {
  let mutation_source =
    "
    mutation CreateUser($name: String!) {
      createUser(name: $name) {
        id
        name
      }
    }
  "

  let assert Ok(operation) = parser.parse(mutation_source)

  let user_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "name",
      schema.NonNullType(schema.NamedType("String", schema.Scalar)),
      [],
      None,
    ),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      Some("Mutation"),
      None,
      dict.from_list([
        #("User", schema.ObjectType("User", user_fields, None)),
        #(
          "Mutation",
          schema.ObjectType(
            "Mutation",
            [
              schema.Field(
                "createUser",
                schema.NamedType("User", schema.Object),
                [
                  schema.InputValue(
                    "name",
                    schema.NonNullType(schema.NamedType("String", schema.Scalar)),
                    None,
                  ),
                ],
                None,
              ),
            ],
            None,
          ),
        ),
      ]),
    )

  let result =
    codegen.generate_operation("create_user", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Mutation function generation")
    }
    Error(_) -> Nil
  }
}

// Test: Generate type with reserved keywords
pub fn generate_with_reserved_keywords_test() {
  let query_source =
    "
    query GetItem {
      item {
        id
        type
        case
        let
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  let item_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field("type", schema.NamedType("String", schema.Scalar), [], None),
    schema.Field("case", schema.NamedType("String", schema.Scalar), [], None),
    schema.Field("let", schema.NamedType("String", schema.Scalar), [], None),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      None,
      None,
      dict.from_list([
        #("Item", schema.ObjectType("Item", item_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "item",
                schema.NamedType("Item", schema.Object),
                [],
                None,
              ),
            ],
            None,
          ),
        ),
      ]),
    )

  let result =
    codegen.generate_operation("get_item", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Type with reserved keywords")
    }
    Error(_) -> Nil
  }
}
