import gleam/option.{type Option, None, Some}
import gleam/result
import squall/internal/error.{type Error}
import squall/internal/parser
import squall/internal/schema

// Gleam type representation
pub type GleamType {
  StringType
  IntType
  FloatType
  BoolType
  ListType(inner: GleamType)
  OptionType(inner: GleamType)
  CustomType(name: String)
}

// Map GraphQL schema type to Gleam type
pub fn graphql_to_gleam(type_ref: schema.TypeRef) -> Result(GleamType, Error) {
  case type_ref {
    schema.NamedType(name, kind) -> map_named_type(name, kind)
    schema.ListType(inner) -> {
      use inner_gleam <- result.try(graphql_to_gleam(inner))
      Ok(ListType(inner_gleam))
    }
    schema.NonNullType(inner) -> graphql_to_gleam(inner)
  }
}

// Map nullable GraphQL type to Gleam type (wraps in Option)
pub fn graphql_to_gleam_nullable(
  type_ref: schema.TypeRef,
) -> Result(GleamType, Error) {
  case type_ref {
    schema.NonNullType(inner) -> graphql_to_gleam(inner)
    _ -> {
      use gleam_type <- result.try(graphql_to_gleam(type_ref))
      Ok(OptionType(gleam_type))
    }
  }
}

// Map named GraphQL type to Gleam type
fn map_named_type(name: String, kind: schema.TypeKind) -> Result(GleamType, Error) {
  case kind {
    schema.Scalar -> map_scalar_type(name)
    schema.Object -> Ok(CustomType(name))
    schema.Interface -> Ok(CustomType(name))
    schema.Union -> Ok(CustomType(name))
    schema.Enum -> Ok(CustomType(name))
    schema.InputObject -> Ok(CustomType(name))
    _ -> Error(error.UnsupportedGraphQLType("type_mapping", name))
  }
}

// Map scalar types
fn map_scalar_type(name: String) -> Result(GleamType, Error) {
  case name {
    "String" -> Ok(StringType)
    "Int" -> Ok(IntType)
    "Float" -> Ok(FloatType)
    "Boolean" -> Ok(BoolType)
    "ID" -> Ok(StringType)
    // Unknown scalars default to String
    _ -> Ok(StringType)
  }
}

// Convert parser TypeRef to schema TypeRef
pub fn parser_type_to_schema_type(
  parser_type: parser.TypeRef,
) -> Result(schema.TypeRef, Error) {
  case parser_type {
    parser.NamedTypeRef(name) -> Ok(schema.NamedType(name, schema.Scalar))
    parser.ListTypeRef(inner) -> {
      use inner_schema <- result.try(parser_type_to_schema_type(inner))
      Ok(schema.ListType(inner_schema))
    }
    parser.NonNullTypeRef(inner) -> {
      use inner_schema <- result.try(parser_type_to_schema_type(inner))
      Ok(schema.NonNullType(inner_schema))
    }
  }
}

// Type checking helpers

pub fn is_string_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    StringType -> True
    _ -> False
  }
}

pub fn is_int_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    IntType -> True
    _ -> False
  }
}

pub fn is_float_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    FloatType -> True
    _ -> False
  }
}

pub fn is_bool_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    BoolType -> True
    _ -> False
  }
}

pub fn is_list_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    ListType(_) -> True
    _ -> False
  }
}

pub fn is_option_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    OptionType(_) -> True
    _ -> False
  }
}

pub fn is_custom_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    CustomType(_) -> True
    _ -> False
  }
}

pub fn get_type_name(gleam_type: GleamType) -> Option(String) {
  case gleam_type {
    CustomType(name) -> Some(name)
    _ -> None
  }
}

// Convert Gleam type to string representation (for code generation)
pub fn to_gleam_type_string(gleam_type: GleamType) -> String {
  case gleam_type {
    StringType -> "String"
    IntType -> "Int"
    FloatType -> "Float"
    BoolType -> "Bool"
    ListType(inner) -> "List(" <> to_gleam_type_string(inner) <> ")"
    OptionType(inner) -> "Option(" <> to_gleam_type_string(inner) <> ")"
    CustomType(name) -> name
  }
}
