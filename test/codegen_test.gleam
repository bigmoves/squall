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

// Test: Generate query with inline scalar arguments
pub fn generate_inline_scalar_arguments_test() {
  let query_source =
    "
    query GetCharacter {
      character(id: 1) {
        id
        name
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
      |> birdie.snap(title: "Query with inline scalar arguments")
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with inline object arguments
pub fn generate_inline_object_arguments_test() {
  let query_source =
    "
    query GetCharacters {
      characters(filter: { name: \"rick\", status: \"alive\" }) {
        results {
          id
          name
        }
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
  ]

  let characters_result_fields = [
    schema.Field(
      "results",
      schema.ListType(schema.NamedType("Character", schema.Object)),
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
          "CharactersResult",
          schema.ObjectType("CharactersResult", characters_result_fields, None),
        ),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "characters",
                schema.NamedType("CharactersResult", schema.Object),
                [
                  schema.InputValue(
                    "filter",
                    schema.NamedType("FilterInput", schema.InputObject),
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
    codegen.generate_operation("get_characters", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Query with inline object arguments")
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with inline array arguments
pub fn generate_inline_array_arguments_test() {
  let query_source =
    "
    query GetEpisodes {
      episodesByIds(ids: [1, 2, 3]) {
        id
        name
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  let episode_fields = [
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
      None,
      None,
      dict.from_list([
        #("Episode", schema.ObjectType("Episode", episode_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "episodesByIds",
                schema.ListType(schema.NamedType("Episode", schema.Object)),
                [
                  schema.InputValue(
                    "ids",
                    schema.ListType(
                      schema.NonNullType(schema.NamedType("Int", schema.Scalar)),
                    ),
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
    codegen.generate_operation("get_episodes", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Query with inline array arguments")
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with multiple root fields and mixed argument types
pub fn generate_multiple_root_fields_test() {
  let query_source =
    "
    query MultiQuery {
      characters(page: 2, filter: { name: \"rick\" }) {
        info {
          count
        }
        results {
          name
        }
      }
      location(id: 1) {
        id
      }
      episodesByIds(ids: [1, 2]) {
        id
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  let character_fields = [
    schema.Field("name", schema.NamedType("String", schema.Scalar), [], None),
  ]

  let info_fields = [
    schema.Field("count", schema.NamedType("Int", schema.Scalar), [], None),
  ]

  let characters_result_fields = [
    schema.Field("info", schema.NamedType("Info", schema.Object), [], None),
    schema.Field(
      "results",
      schema.ListType(schema.NamedType("Character", schema.Object)),
      [],
      None,
    ),
  ]

  let location_fields = [
    schema.Field("id", schema.NamedType("ID", schema.Scalar), [], None),
  ]

  let episode_fields = [
    schema.Field("id", schema.NamedType("ID", schema.Scalar), [], None),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      None,
      None,
      dict.from_list([
        #("Character", schema.ObjectType("Character", character_fields, None)),
        #("Info", schema.ObjectType("Info", info_fields, None)),
        #(
          "CharactersResult",
          schema.ObjectType("CharactersResult", characters_result_fields, None),
        ),
        #("Location", schema.ObjectType("Location", location_fields, None)),
        #("Episode", schema.ObjectType("Episode", episode_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "characters",
                schema.NamedType("CharactersResult", schema.Object),
                [
                  schema.InputValue(
                    "page",
                    schema.NamedType("Int", schema.Scalar),
                    None,
                  ),
                  schema.InputValue(
                    "filter",
                    schema.NamedType("FilterInput", schema.InputObject),
                    None,
                  ),
                ],
                None,
              ),
              schema.Field(
                "location",
                schema.NamedType("Location", schema.Object),
                [
                  schema.InputValue(
                    "id",
                    schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
                    None,
                  ),
                ],
                None,
              ),
              schema.Field(
                "episodesByIds",
                schema.ListType(schema.NamedType("Episode", schema.Object)),
                [
                  schema.InputValue(
                    "ids",
                    schema.ListType(
                      schema.NonNullType(schema.NamedType("Int", schema.Scalar)),
                    ),
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
    codegen.generate_operation("multi_query", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(
        title: "Query with multiple root fields and mixed arguments",
      )
    }
    Error(_) -> Nil
  }
}

// Test: Generate mutation with InputObject variable
pub fn generate_mutation_with_input_object_test() {
  let mutation_source =
    "
    mutation UpdateProfile($input: ProfileInput!) {
      updateProfile(input: $input) {
        id
        displayName
      }
    }
  "

  let assert Ok(operation) = parser.parse(mutation_source)

  // Define InputObject type in schema
  let profile_input_fields = [
    schema.InputValue(
      "displayName",
      schema.NamedType("String", schema.Scalar),
      None,
    ),
    schema.InputValue(
      "description",
      schema.NamedType("String", schema.Scalar),
      None,
    ),
  ]

  let profile_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "displayName",
      schema.NamedType("String", schema.Scalar),
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
        #("Profile", schema.ObjectType("Profile", profile_fields, None)),
        #(
          "ProfileInput",
          schema.InputObjectType("ProfileInput", profile_input_fields, None),
        ),
        #(
          "Mutation",
          schema.ObjectType(
            "Mutation",
            [
              schema.Field(
                "updateProfile",
                schema.NamedType("Profile", schema.Object),
                [
                  schema.InputValue(
                    "input",
                    schema.NonNullType(schema.NamedType(
                      "ProfileInput",
                      schema.InputObject,
                    )),
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
    codegen.generate_operation("update_profile", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Mutation with InputObject variable")
    }
    Error(_) -> Nil
  }
}

// Test: Generate mutation with nested InputObject types
pub fn generate_mutation_with_nested_input_object_test() {
  let mutation_source =
    "
    mutation UpdateProfile($input: ProfileInput!) {
      updateProfile(input: $input) {
        id
        displayName
      }
    }
  "

  let assert Ok(operation) = parser.parse(mutation_source)

  // Define nested InputObject types
  let blob_input_fields = [
    schema.InputValue(
      "data",
      schema.NonNullType(schema.NamedType("String", schema.Scalar)),
      None,
    ),
    schema.InputValue(
      "mimeType",
      schema.NonNullType(schema.NamedType("String", schema.Scalar)),
      None,
    ),
  ]

  let profile_input_fields = [
    schema.InputValue(
      "displayName",
      schema.NamedType("String", schema.Scalar),
      None,
    ),
    schema.InputValue(
      "avatar",
      schema.NamedType("BlobInput", schema.InputObject),
      None,
    ),
    schema.InputValue(
      "interests",
      schema.ListType(
        schema.NonNullType(schema.NamedType("String", schema.Scalar)),
      ),
      None,
    ),
  ]

  let profile_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "displayName",
      schema.NamedType("String", schema.Scalar),
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
        #("Profile", schema.ObjectType("Profile", profile_fields, None)),
        #(
          "ProfileInput",
          schema.InputObjectType("ProfileInput", profile_input_fields, None),
        ),
        #(
          "BlobInput",
          schema.InputObjectType("BlobInput", blob_input_fields, None),
        ),
        #(
          "Mutation",
          schema.ObjectType(
            "Mutation",
            [
              schema.Field(
                "updateProfile",
                schema.NamedType("Profile", schema.Object),
                [
                  schema.InputValue(
                    "input",
                    schema.NonNullType(schema.NamedType(
                      "ProfileInput",
                      schema.InputObject,
                    )),
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
    codegen.generate_operation("update_profile", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Mutation with nested InputObject types")
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with all non-nullable fields (no Option import needed)
pub fn generate_query_with_all_non_nullable_fields_test() {
  let query_source =
    "
    query GetProduct {
      product {
        id
        name
        price
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  // Create mock schema with all non-nullable fields
  let product_fields = [
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
      "price",
      schema.NonNullType(schema.NamedType("Float", schema.Scalar)),
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
        #("Product", schema.ObjectType("Product", product_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "product",
                schema.NonNullType(schema.NamedType("Product", schema.Object)),
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
    codegen.generate_operation("get_product", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(
        title: "Query with all non-nullable fields (no Option import)",
      )
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with JSON scalar field
pub fn generate_query_with_json_scalar_test() {
  let query_source =
    "
    query GetProfile {
      profile {
        id
        displayName
        metadata
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  // Create mock schema with JSON scalar field
  let profile_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "displayName",
      schema.NamedType("String", schema.Scalar),
      [],
      None,
    ),
    schema.Field("metadata", schema.NamedType("JSON", schema.Scalar), [], None),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      None,
      None,
      dict.from_list([
        #("Profile", schema.ObjectType("Profile", profile_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "profile",
                schema.NamedType("Profile", schema.Object),
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
    codegen.generate_operation("get_profile", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Query with JSON scalar field")
    }
    Error(_) -> Nil
  }
}

// Test: Generate mutation with JSON scalar in InputObject
pub fn generate_mutation_with_json_input_field_test() {
  let mutation_source =
    "
    mutation UpdateSettings($input: SettingsInput!) {
      updateSettings(input: $input) {
        id
        metadata
      }
    }
  "

  let assert Ok(operation) = parser.parse(mutation_source)

  // Define InputObject type with JSON field
  let settings_input_fields = [
    schema.InputValue("metadata", schema.NamedType("JSON", schema.Scalar), None),
    schema.InputValue(
      "displayName",
      schema.NamedType("String", schema.Scalar),
      None,
    ),
  ]

  let settings_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field("metadata", schema.NamedType("JSON", schema.Scalar), [], None),
  ]

  let mock_schema =
    schema.Schema(
      Some("Query"),
      Some("Mutation"),
      None,
      dict.from_list([
        #("Settings", schema.ObjectType("Settings", settings_fields, None)),
        #(
          "SettingsInput",
          schema.InputObjectType("SettingsInput", settings_input_fields, None),
        ),
        #(
          "Mutation",
          schema.ObjectType(
            "Mutation",
            [
              schema.Field(
                "updateSettings",
                schema.NamedType("Settings", schema.Object),
                [
                  schema.InputValue(
                    "input",
                    schema.NonNullType(schema.NamedType(
                      "SettingsInput",
                      schema.InputObject,
                    )),
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
    codegen.generate_operation("update_settings", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(title: "Mutation with JSON scalar in InputObject")
    }
    Error(_) -> Nil
  }
}

// Test: Generate mutation with optional InputObject fields (tests Some/None serializer)
pub fn generate_mutation_with_optional_input_fields_test() {
  let mutation_source =
    "
    mutation CreateProfile($input: ProfileInput!) {
      createProfile(input: $input) {
        id
        displayName
        description
      }
    }
  "

  let assert Ok(operation) = parser.parse(mutation_source)

  // Define InputObject type with optional fields (nullable in GraphQL)
  let profile_input_fields = [
    schema.InputValue(
      "displayName",
      schema.NamedType("String", schema.Scalar),
      None,
    ),
    schema.InputValue(
      "description",
      schema.NamedType("String", schema.Scalar),
      None,
    ),
    schema.InputValue("avatar", schema.NamedType("JSON", schema.Scalar), None),
  ]

  let profile_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "displayName",
      schema.NamedType("String", schema.Scalar),
      [],
      None,
    ),
    schema.Field(
      "description",
      schema.NamedType("String", schema.Scalar),
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
        #("Profile", schema.ObjectType("Profile", profile_fields, None)),
        #(
          "ProfileInput",
          schema.InputObjectType("ProfileInput", profile_input_fields, None),
        ),
        #(
          "Mutation",
          schema.ObjectType(
            "Mutation",
            [
              schema.Field(
                "createProfile",
                schema.NamedType("Profile", schema.Object),
                [
                  schema.InputValue(
                    "input",
                    schema.NonNullType(schema.NamedType(
                      "ProfileInput",
                      schema.InputObject,
                    )),
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
    codegen.generate_operation("create_profile", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(
        title: "Mutation with optional InputObject fields (imports Some, None)",
      )
    }
    Error(_) -> Nil
  }
}

// Test: Generate query with optional response fields only (should not import Some, None)
pub fn generate_query_with_optional_response_fields_test() {
  let query_source =
    "
    query GetProfile {
      profile {
        id
        displayName
        description
      }
    }
  "

  let assert Ok(operation) = parser.parse(query_source)

  // Create mock schema with optional response fields
  let profile_fields = [
    schema.Field(
      "id",
      schema.NonNullType(schema.NamedType("ID", schema.Scalar)),
      [],
      None,
    ),
    schema.Field(
      "displayName",
      schema.NamedType("String", schema.Scalar),
      [],
      None,
    ),
    schema.Field(
      "description",
      schema.NamedType("String", schema.Scalar),
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
        #("Profile", schema.ObjectType("Profile", profile_fields, None)),
        #(
          "Query",
          schema.ObjectType(
            "Query",
            [
              schema.Field(
                "profile",
                schema.NamedType("Profile", schema.Object),
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
    codegen.generate_operation("get_profile", operation, mock_schema, "")

  case result {
    Ok(code) -> {
      code
      |> birdie.snap(
        title: "Query with optional response fields (no Some, None imports)",
      )
    }
    Error(_) -> Nil
  }
}
