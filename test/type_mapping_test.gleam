import gleam/option.{Some}
import gleeunit/should
import squall/internal/parser
import squall/internal/schema
import squall/internal/type_mapping

// Test: Map GraphQL String to Gleam String
pub fn map_string_type_test() {
  let graphql_type = schema.NamedType("String", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_string_type(gleam_type)
  |> should.be_true()
}

// Test: Map GraphQL Int to Gleam Int
pub fn map_int_type_test() {
  let graphql_type = schema.NamedType("Int", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_int_type(gleam_type)
  |> should.be_true()
}

// Test: Map GraphQL Float to Gleam Float
pub fn map_float_type_test() {
  let graphql_type = schema.NamedType("Float", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_float_type(gleam_type)
  |> should.be_true()
}

// Test: Map GraphQL Boolean to Gleam Bool
pub fn map_boolean_type_test() {
  let graphql_type = schema.NamedType("Boolean", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_bool_type(gleam_type)
  |> should.be_true()
}

// Test: Map GraphQL ID to Gleam String
pub fn map_id_type_test() {
  let graphql_type = schema.NamedType("ID", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_string_type(gleam_type)
  |> should.be_true()
}

// Test: Map nullable String to Option(String)
pub fn map_nullable_string_test() {
  let graphql_type = schema.NamedType("String", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam_nullable(
      graphql_type,
      type_mapping.OutputContext,
    )

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_option_type(gleam_type)
  |> should.be_true()
}

// Test: Map NonNull String to String (not Option)
pub fn map_non_null_string_test() {
  let inner = schema.NamedType("String", schema.Scalar)
  let graphql_type = schema.NonNullType(inner)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_string_type(gleam_type)
  |> should.be_true()

  type_mapping.is_option_type(gleam_type)
  |> should.be_false()
}

// Test: Map List of Strings
pub fn map_list_of_strings_test() {
  let inner = schema.NamedType("String", schema.Scalar)
  let graphql_type = schema.ListType(inner)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_list_type(gleam_type)
  |> should.be_true()
}

// Test: Map NonNull List of NonNull Strings ([String!]!)
pub fn map_non_null_list_non_null_strings_test() {
  let string_type = schema.NamedType("String", schema.Scalar)
  let non_null_string = schema.NonNullType(string_type)
  let list_type = schema.ListType(non_null_string)
  let graphql_type = schema.NonNullType(list_type)

  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_list_type(gleam_type)
  |> should.be_true()
}

// Test: Map custom object type
pub fn map_custom_object_type_test() {
  let graphql_type = schema.NamedType("Character", schema.Object)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_custom_type(gleam_type)
  |> should.be_true()

  type_mapping.get_type_name(gleam_type)
  |> should.equal(Some("Character"))
}

// Test: Parse variable type from parser TypeRef
pub fn parse_variable_type_test() {
  let parser_type = parser.NonNullTypeRef(parser.NamedTypeRef("ID"))
  let result = type_mapping.parser_type_to_schema_type(parser_type)

  should.be_ok(result)
  let assert Ok(schema_type) = result

  case schema_type {
    schema.NonNullType(schema.NamedType("ID", _)) -> should.equal(True, True)
    _ -> should.equal(True, False)
  }
}

// Test: Unsupported custom scalar
pub fn unsupported_scalar_test() {
  let graphql_type = schema.NamedType("DateTime", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  // Should map unknown scalars to String by default
  should.be_ok(result)
}

// Test: Map GraphQL JSON scalar to Gleam Dynamic (OutputContext)
pub fn map_json_type_output_test() {
  let graphql_type = schema.NamedType("JSON", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_dynamic_type(gleam_type)
  |> should.be_true()
}

// Test: Map GraphQL JSON scalar to Gleam json.Json (InputContext)
pub fn map_json_type_input_test() {
  let graphql_type = schema.NamedType("JSON", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.InputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_json_type(gleam_type)
  |> should.be_true()
}

// Test: Map nullable JSON to Option(Dynamic) (OutputContext)
pub fn map_nullable_json_output_test() {
  let graphql_type = schema.NamedType("JSON", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam_nullable(
      graphql_type,
      type_mapping.OutputContext,
    )

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_option_type(gleam_type)
  |> should.be_true()
}

// Test: Map nullable JSON to Option(json.Json) (InputContext)
pub fn map_nullable_json_input_test() {
  let graphql_type = schema.NamedType("JSON", schema.Scalar)
  let result =
    type_mapping.graphql_to_gleam_nullable(
      graphql_type,
      type_mapping.InputContext,
    )

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_option_type(gleam_type)
  |> should.be_true()

  // Inner type should be json.Json
  case gleam_type {
    type_mapping.OptionType(inner) -> {
      type_mapping.is_json_type(inner)
      |> should.be_true()
    }
    _ -> should.equal(True, False)
  }
}

// Test: Map NonNull JSON to Dynamic in OutputContext (not Option)
pub fn map_non_null_json_output_test() {
  let inner = schema.NamedType("JSON", schema.Scalar)
  let graphql_type = schema.NonNullType(inner)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.OutputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_dynamic_type(gleam_type)
  |> should.be_true()

  type_mapping.is_option_type(gleam_type)
  |> should.be_false()
}

// Test: Map NonNull JSON to json.Json in InputContext (not Option)
pub fn map_non_null_json_input_test() {
  let inner = schema.NamedType("JSON", schema.Scalar)
  let graphql_type = schema.NonNullType(inner)
  let result =
    type_mapping.graphql_to_gleam(graphql_type, type_mapping.InputContext)

  should.be_ok(result)
  let assert Ok(gleam_type) = result

  type_mapping.is_json_type(gleam_type)
  |> should.be_true()

  type_mapping.is_option_type(gleam_type)
  |> should.be_false()
}

// Test: to_gleam_type_string for DynamicType
pub fn dynamic_type_string_test() {
  let gleam_type = type_mapping.DynamicType
  let type_string = type_mapping.to_gleam_type_string(gleam_type)

  should.equal(type_string, "Dynamic")
}
