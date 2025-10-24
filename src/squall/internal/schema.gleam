import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import squall/internal/error.{type Error}

// GraphQL type kinds
pub type TypeKind {
  Scalar
  Object
  Interface
  Union
  Enum
  InputObject
  List
  NonNull
}

// GraphQL type reference
pub type TypeRef {
  NamedType(name: String, kind: TypeKind)
  ListType(of_type: TypeRef)
  NonNullType(of_type: TypeRef)
}

// GraphQL field definition
pub type Field {
  Field(
    name: String,
    type_ref: TypeRef,
    args: List(InputValue),
    description: Option(String),
  )
}

// GraphQL input value (for arguments and input fields)
pub type InputValue {
  InputValue(name: String, type_ref: TypeRef, description: Option(String))
}

// GraphQL type definition
pub type Type {
  ScalarType(name: String, description: Option(String))
  ObjectType(name: String, fields: List(Field), description: Option(String))
  InterfaceType(name: String, fields: List(Field), description: Option(String))
  UnionType(
    name: String,
    possible_types: List(String),
    description: Option(String),
  )
  EnumType(name: String, enum_values: List(String), description: Option(String))
  InputObjectType(
    name: String,
    input_fields: List(InputValue),
    description: Option(String),
  )
}

// Schema data structure
pub type Schema {
  Schema(
    query_type: Option(String),
    mutation_type: Option(String),
    subscription_type: Option(String),
    types: Dict(String, Type),
  )
}

// Helper functions for decoding with new API
fn field(
  dyn: Dynamic,
  name: String,
  decoder: decode.Decoder(t),
) -> Result(t, List(decode.DecodeError)) {
  let field_decoder = {
    use value <- decode.field(name, decoder)
    decode.success(value)
  }
  decode.run(dyn, field_decoder)
}

fn optional_field(
  dyn: Dynamic,
  name: String,
  decoder: decode.Decoder(t),
) -> Option(t) {
  field(dyn, name, decoder)
  |> result.map(Some)
  |> result.unwrap(None)
}

fn decode_errors_to_string(errs: List(decode.DecodeError)) -> String {
  errs
  |> list.map(fn(_err) { "Decode error" })
  |> list.reduce(fn(a, b) { a <> ", " <> b })
  |> result.unwrap("unknown error")
}

// Parse introspection response JSON into Schema
pub fn parse_introspection_response(json_str: String) -> Result(Schema, Error) {
  case json.parse(from: json_str, using: decode.dynamic) {
    Ok(dyn) -> decode_schema(dyn)
    Error(json.UnableToDecode(errs)) ->
      Error(error.InvalidSchemaResponse(
        "Decode error: " <> decode_errors_to_string(errs),
      ))
    Error(_) -> Error(error.InvalidSchemaResponse("Invalid JSON"))
  }
}

fn decode_schema(dyn: Dynamic) -> Result(Schema, Error) {
  // Extract data.__schema
  use data <- result.try(
    field(dyn, "data", decode.dynamic)
    |> result.map_error(to_schema_error),
  )
  use schema_obj <- result.try(
    field(data, "__schema", decode.dynamic)
    |> result.map_error(to_schema_error),
  )

  // Parse optional query type
  let query_type = case field(schema_obj, "queryType", decode.dynamic) {
    Ok(qt) -> optional_field(qt, "name", decode.string)
    Error(_) -> None
  }

  // Parse optional mutation type
  let mutation_type = case field(schema_obj, "mutationType", decode.dynamic) {
    Ok(mt) -> optional_field(mt, "name", decode.string)
    Error(_) -> None
  }

  // Parse optional subscription type
  let subscription_type = case
    field(schema_obj, "subscriptionType", decode.dynamic)
  {
    Ok(st) -> optional_field(st, "name", decode.string)
    Error(_) -> None
  }

  // Parse types array
  use types_list_dyn <- result.try(
    field(schema_obj, "types", decode.list(of: decode.dynamic))
    |> result.map_error(to_schema_error),
  )

  // Decode each type
  let types_result =
    types_list_dyn
    |> list.try_map(decode_type)

  use types_list <- result.try(types_result)

  // Build types dictionary
  let types =
    types_list
    |> list.map(fn(t) { #(get_type_name(t), t) })
    |> dict.from_list

  Ok(Schema(query_type, mutation_type, subscription_type, types))
}

fn decode_type(dyn: Dynamic) -> Result(Type, Error) {
  use name <- result.try(
    field(dyn, "name", decode.string)
    |> result.map_error(to_schema_error),
  )
  use kind <- result.try(
    field(dyn, "kind", decode.string)
    |> result.map_error(to_schema_error),
  )

  let description = optional_field(dyn, "description", decode.string)

  case kind {
    "SCALAR" -> Ok(ScalarType(name, description))

    "OBJECT" | "INTERFACE" -> {
      let fields = case field(dyn, "fields", decode.list(of: decode.dynamic)) {
        Ok(fields_dyn) -> {
          list.try_map(fields_dyn, decode_field)
          |> result.unwrap([])
        }
        Error(_) -> []
      }

      case kind {
        "OBJECT" -> Ok(ObjectType(name, fields, description))
        _ -> Ok(InterfaceType(name, fields, description))
      }
    }

    "UNION" -> {
      let possible_types = case
        field(dyn, "possibleTypes", decode.list(of: decode.dynamic))
      {
        Ok(types_dyn) -> {
          list.try_map(types_dyn, fn(d) {
            field(d, "name", decode.string)
            |> result.map_error(to_schema_error)
          })
          |> result.unwrap([])
        }
        Error(_) -> []
      }

      Ok(UnionType(name, possible_types, description))
    }

    "ENUM" -> {
      let enum_values = case
        field(dyn, "enumValues", decode.list(of: decode.dynamic))
      {
        Ok(values_dyn) -> {
          list.try_map(values_dyn, fn(d) {
            field(d, "name", decode.string)
            |> result.map_error(to_schema_error)
          })
          |> result.unwrap([])
        }
        Error(_) -> []
      }

      Ok(EnumType(name, enum_values, description))
    }

    "INPUT_OBJECT" -> {
      let input_fields = case
        field(dyn, "inputFields", decode.list(of: decode.dynamic))
      {
        Ok(fields_dyn) -> {
          list.try_map(fields_dyn, decode_input_value)
          |> result.unwrap([])
        }
        Error(_) -> []
      }

      Ok(InputObjectType(name, input_fields, description))
    }

    _ -> Ok(ScalarType(name, description))
  }
}

fn decode_field(dyn: Dynamic) -> Result(Field, Error) {
  use name <- result.try(
    field(dyn, "name", decode.string)
    |> result.map_error(to_schema_error),
  )
  use type_dyn <- result.try(
    field(dyn, "type", decode.dynamic)
    |> result.map_error(to_schema_error),
  )
  use type_ref <- result.try(decode_type_ref(type_dyn))

  let args = case field(dyn, "args", decode.list(of: decode.dynamic)) {
    Ok(args_dyn) -> {
      list.try_map(args_dyn, decode_input_value)
      |> result.unwrap([])
    }
    Error(_) -> []
  }

  let description = optional_field(dyn, "description", decode.string)

  Ok(Field(name, type_ref, args, description))
}

fn decode_input_value(dyn: Dynamic) -> Result(InputValue, Error) {
  use name <- result.try(
    field(dyn, "name", decode.string)
    |> result.map_error(to_schema_error),
  )
  use type_dyn <- result.try(
    field(dyn, "type", decode.dynamic)
    |> result.map_error(to_schema_error),
  )
  use type_ref <- result.try(decode_type_ref(type_dyn))

  let description = optional_field(dyn, "description", decode.string)

  Ok(InputValue(name, type_ref, description))
}

fn decode_type_ref(dyn: Dynamic) -> Result(TypeRef, Error) {
  use kind <- result.try(
    field(dyn, "kind", decode.string)
    |> result.map_error(to_schema_error),
  )

  case kind {
    "NON_NULL" -> {
      use of_type_dyn <- result.try(
        field(dyn, "ofType", decode.dynamic)
        |> result.map_error(to_schema_error),
      )
      use of_type <- result.try(decode_type_ref(of_type_dyn))
      Ok(NonNullType(of_type))
    }

    "LIST" -> {
      use of_type_dyn <- result.try(
        field(dyn, "ofType", decode.dynamic)
        |> result.map_error(to_schema_error),
      )
      use of_type <- result.try(decode_type_ref(of_type_dyn))
      Ok(ListType(of_type))
    }

    _ -> {
      use name <- result.try(
        field(dyn, "name", decode.string)
        |> result.map_error(to_schema_error),
      )
      Ok(NamedType(name, kind_from_string(kind)))
    }
  }
}

fn kind_from_string(s: String) -> TypeKind {
  case s {
    "SCALAR" -> Scalar
    "OBJECT" -> Object
    "INTERFACE" -> Interface
    "UNION" -> Union
    "ENUM" -> Enum
    "INPUT_OBJECT" -> InputObject
    "LIST" -> List
    "NON_NULL" -> NonNull
    _ -> Scalar
  }
}

fn to_schema_error(errs: List(decode.DecodeError)) -> Error {
  error.InvalidSchemaResponse("Decode error: " <> decode_errors_to_string(errs))
}

// Helper functions for tests

pub fn get_type_count(schema: Schema) -> Int {
  dict.size(schema.types)
}

pub fn find_type(schema: Schema, name: String) -> Result(Type, Error) {
  dict.get(schema.types, name)
  |> result.map_error(fn(_) {
    error.InvalidSchemaResponse("Type not found: " <> name)
  })
}

pub fn get_type_name(type_: Type) -> String {
  case type_ {
    ScalarType(name, _) -> name
    ObjectType(name, _, _) -> name
    InterfaceType(name, _, _) -> name
    UnionType(name, _, _) -> name
    EnumType(name, _, _) -> name
    InputObjectType(name, _, _) -> name
  }
}

pub fn get_type_fields(type_: Type) -> List(Field) {
  case type_ {
    ObjectType(_, fields, _) -> fields
    InterfaceType(_, fields, _) -> fields
    _ -> []
  }
}

pub fn get_query_type_name(schema: Schema) -> Result(String, Error) {
  case schema.query_type {
    Some(name) -> Ok(name)
    None -> Error(error.InvalidSchemaResponse("No query type defined"))
  }
}
