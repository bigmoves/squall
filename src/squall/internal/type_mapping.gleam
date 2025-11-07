import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import squall/internal/error.{type Error}
import squall/internal/schema

// Type context for distinguishing input vs output types
pub type TypeContext {
  InputContext
  OutputContext
}

// Gleam type representation
pub type GleamType {
  StringType
  IntType
  FloatType
  BoolType
  DynamicType
  JsonType
  ListType(inner: GleamType)
  OptionType(inner: GleamType)
  CustomType(name: String)
}

// Map GraphQL schema type to Gleam type
pub fn graphql_to_gleam(
  type_ref: schema.TypeRef,
  context: TypeContext,
) -> Result(GleamType, Error) {
  case type_ref {
    schema.NamedType(name, kind) -> map_named_type(name, kind, context)
    schema.ListType(inner) -> {
      use inner_gleam <- result.try(graphql_to_gleam(inner, context))
      Ok(ListType(inner_gleam))
    }
    schema.NonNullType(inner) -> graphql_to_gleam(inner, context)
  }
}

// Map nullable GraphQL type to Gleam type (wraps in Option)
pub fn graphql_to_gleam_nullable(
  type_ref: schema.TypeRef,
  context: TypeContext,
) -> Result(GleamType, Error) {
  case type_ref {
    schema.NonNullType(inner) -> graphql_to_gleam(inner, context)
    _ -> {
      use gleam_type <- result.try(graphql_to_gleam(type_ref, context))
      Ok(OptionType(gleam_type))
    }
  }
}

// Map named GraphQL type to Gleam type
fn map_named_type(
  name: String,
  kind: schema.TypeKind,
  context: TypeContext,
) -> Result(GleamType, Error) {
  case kind {
    schema.Scalar -> map_scalar_type(name, context)
    schema.Object -> Ok(CustomType(name))
    schema.Interface -> Ok(CustomType(name))
    schema.Union -> Ok(CustomType(name))
    schema.Enum -> Ok(CustomType(name))
    schema.InputObject -> Ok(CustomType(name))
    _ -> Error(error.UnsupportedGraphQLType("type_mapping", name))
  }
}

// Map scalar types
fn map_scalar_type(
  name: String,
  context: TypeContext,
) -> Result(GleamType, Error) {
  case name {
    "String" -> Ok(StringType)
    "Int" -> Ok(IntType)
    "Float" -> Ok(FloatType)
    "Boolean" -> Ok(BoolType)
    "ID" -> Ok(StringType)
    "JSON" ->
      case context {
        InputContext -> Ok(JsonType)
        OutputContext -> Ok(DynamicType)
      }
    // Unknown scalars default to String
    _ -> Ok(StringType)
  }
}

// Parse a GraphQL type string (e.g., "String!", "[Int]", "[User!]!") into a schema TypeRef
pub fn parse_type_string(type_str: String) -> Result(schema.TypeRef, Error) {
  parse_type_string_helper(type_str, 0).0
}

// Helper for parsing type strings recursively
// Returns (Result, position_after_parsing)
fn parse_type_string_helper(
  type_str: String,
  pos: Int,
) -> #(Result(schema.TypeRef, Error), Int) {
  let chars = string.to_graphemes(type_str)

  // Skip whitespace
  let pos = skip_whitespace(chars, pos)

  case list_at(chars, pos) {
    Some("[") -> {
      // List type: [InnerType]
      let #(inner_result, pos_after_inner) =
        parse_type_string_helper(type_str, pos + 1)

      case inner_result {
        Ok(inner) -> {
          // Expect closing ]
          let pos = skip_whitespace(chars, pos_after_inner)
          case list_at(chars, pos) {
            Some("]") -> {
              let pos = pos + 1
              // Check for non-null marker
              let pos_after_space = skip_whitespace(chars, pos)
              case list_at(chars, pos_after_space) {
                Some("!") -> #(
                  Ok(schema.NonNullType(schema.ListType(inner))),
                  pos_after_space + 1,
                )
                _ -> #(Ok(schema.ListType(inner)), pos)
              }
            }
            _ -> #(
              Error(error.InvalidGraphQLSyntax("type_string", 0, "Expected ']'")),
              pos,
            )
          }
        }
        Error(e) -> #(Error(e), pos_after_inner)
      }
    }

    Some(char) -> {
      case is_alpha(char) {
        True -> {
          // Named type
          let #(name, pos_after_name) = read_name(chars, pos)

          // Check for non-null marker
          let pos_after_space = skip_whitespace(chars, pos_after_name)
          case list_at(chars, pos_after_space) {
            Some("!") -> #(
              Ok(schema.NonNullType(schema.NamedType(name, schema.Scalar))),
              pos_after_space + 1,
            )
            _ -> #(Ok(schema.NamedType(name, schema.Scalar)), pos_after_name)
          }
        }
        False -> #(
          Error(error.InvalidGraphQLSyntax("type_string", 0, "Invalid type")),
          pos,
        )
      }
    }

    None -> #(
      Error(error.InvalidGraphQLSyntax("type_string", 0, "Invalid type")),
      pos,
    )
  }
}

fn skip_whitespace(chars: List(String), pos: Int) -> Int {
  case list_at(chars, pos) {
    Some(" ") | Some("\t") | Some("\n") | Some("\r") ->
      skip_whitespace(chars, pos + 1)
    _ -> pos
  }
}

fn list_at(lst: List(a), index: Int) -> Option(a) {
  lst
  |> list.drop(index)
  |> list.first
  |> option.from_result
}

fn is_alpha(char: String) -> Bool {
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
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z"
    | "_" -> True
    _ -> False
  }
}

fn is_alphanumeric(char: String) -> Bool {
  is_alpha(char)
  || case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn read_name(chars: List(String), pos: Int) -> #(String, Int) {
  read_name_helper(chars, pos, "")
}

fn read_name_helper(
  chars: List(String),
  pos: Int,
  acc: String,
) -> #(String, Int) {
  case list_at(chars, pos) {
    Some(char) -> {
      case is_alphanumeric(char) {
        True -> read_name_helper(chars, pos + 1, acc <> char)
        False -> #(acc, pos)
      }
    }
    None -> #(acc, pos)
  }
}

// Convert type string to schema TypeRef with schema lookup for accurate kinds
pub fn parse_type_string_with_schema(
  type_str: String,
  schema_types: dict.Dict(String, schema.Type),
) -> Result(schema.TypeRef, Error) {
  use type_ref <- result.try(parse_type_string(type_str))

  // Update the kind based on schema lookup
  update_type_ref_kinds(type_ref, schema_types)
}

fn update_type_ref_kinds(
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
) -> Result(schema.TypeRef, Error) {
  case type_ref {
    schema.NamedType(name, _) -> {
      let kind = case dict.get(schema_types, name) {
        Ok(schema.ScalarType(_, _)) -> schema.Scalar
        Ok(schema.ObjectType(_, _, _)) -> schema.Object
        Ok(schema.InterfaceType(_, _, _)) -> schema.Interface
        Ok(schema.UnionType(_, _, _)) -> schema.Union
        Ok(schema.EnumType(_, _, _)) -> schema.Enum
        Ok(schema.InputObjectType(_, _, _)) -> schema.InputObject
        Error(_) -> schema.Scalar
      }
      Ok(schema.NamedType(name, kind))
    }
    schema.ListType(inner) -> {
      use updated_inner <- result.try(update_type_ref_kinds(inner, schema_types))
      Ok(schema.ListType(updated_inner))
    }
    schema.NonNullType(inner) -> {
      use updated_inner <- result.try(update_type_ref_kinds(inner, schema_types))
      Ok(schema.NonNullType(updated_inner))
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

pub fn is_dynamic_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    DynamicType -> True
    _ -> False
  }
}

pub fn is_json_type(gleam_type: GleamType) -> Bool {
  case gleam_type {
    JsonType -> True
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
    DynamicType -> "Dynamic"
    JsonType -> "json.Json"
    ListType(inner) -> "List(" <> to_gleam_type_string(inner) <> ")"
    OptionType(inner) -> "Option(" <> to_gleam_type_string(inner) <> ")"
    CustomType(name) -> name
  }
}
