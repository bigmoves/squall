import glam/doc.{type Document}
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import squall/internal/error.{type Error}
import squall/internal/graphql_ast
import squall/internal/schema
import squall/internal/type_mapping
import swell/parser

pub type GeneratedCode {
  GeneratedCode(
    function_code: String,
    type_definitions: List(String),
    imports: List(String),
  )
}

// Type to track nested types that need to be generated
type NestedTypeInfo {
  NestedTypeInfo(
    type_name: String,
    fields: List(#(String, schema.TypeRef)),
    field_types: dict.Dict(String, schema.Type),
  )
}

// Type to track input types that need to be generated
type InputTypeInfo {
  InputTypeInfo(
    type_name: String,
    input_fields: List(schema.InputValue),
    field_types: dict.Dict(String, schema.Type),
  )
}

// Type to track enum types that need to be generated
type EnumTypeInfo {
  EnumTypeInfo(type_name: String, enum_values: List(String))
}

// --- CONSTANTS ---------------------------------------------------------------

const indent = 2

// Gleam reserved keywords that need to be sanitized
const reserved_keywords = [
  "as", "assert", "case", "const", "external", "fn", "if", "import", "let",
  "opaque", "pub", "todo", "try", "type", "use",
]

// --- DOCUMENT HELPERS --------------------------------------------------------

/// A pretty printed function call.
fn call_doc(function: String, args: List(Document)) -> Document {
  [doc.from_string(function), comma_list("(", args, ")") |> doc.group]
  |> doc.concat
  |> doc.group
}

/// A comma separated list of items with some given open and closed delimiters.
fn comma_list(open: String, content: List(Document), close: String) -> Document {
  case content {
    [] -> doc.from_string(open <> close)
    _ ->
      [
        doc.from_string(open),
        [
          doc.soft_break,
          doc.join(content, doc.break(", ", ",")),
        ]
          |> doc.concat
          |> doc.nest(by: indent),
        doc.break("", ","),
        doc.from_string(close),
      ]
      |> doc.concat
  }
}

/// A pretty printed Gleam block.
fn block(body: List(Document)) -> Document {
  [
    doc.from_string("{"),
    doc.line |> doc.nest(by: indent),
    body
      |> doc.join(with: doc.line)
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("}"),
  ]
  |> doc.concat
}

/// A pretty printed Gleam string with proper escaping.
fn string_doc(content: String) -> Document {
  let escaped_string =
    content
    |> string.replace(each: "\\", with: "\\\\")
    |> string.replace(each: "\"", with: "\\\"")
    |> string.replace(each: "\n", with: "\\n")
    |> doc.from_string

  [doc.from_string("\""), escaped_string, doc.from_string("\"")]
  |> doc.concat
}

/// Sanitize field names by converting to snake_case and appending underscore to reserved keywords
fn sanitize_field_name(name: String) -> String {
  // Handle GraphQL introspection fields by removing __ prefix
  let cleaned = case string.starts_with(name, "__") {
    True -> string.drop_start(name, 2)
    False -> name
  }
  let snake_cased = snake_case(cleaned)
  case list.contains(reserved_keywords, snake_cased) {
    True -> snake_cased <> "_"
    False -> snake_cased
  }
}

/// Recursively detect if Option types are used
fn detect_option_usage_in_gleam_type(gleam_type: type_mapping.GleamType) -> Bool {
  case gleam_type {
    type_mapping.StringType
    | type_mapping.IntType
    | type_mapping.FloatType
    | type_mapping.BoolType
    | type_mapping.DynamicType
    | type_mapping.JsonType
    | type_mapping.CustomType(_) -> False
    type_mapping.ListType(inner) -> detect_option_usage_in_gleam_type(inner)
    type_mapping.OptionType(_inner) -> True
  }
}

fn detect_dynamic_usage_in_gleam_type(
  gleam_type: type_mapping.GleamType,
) -> Bool {
  case gleam_type {
    type_mapping.StringType
    | type_mapping.IntType
    | type_mapping.FloatType
    | type_mapping.BoolType
    | type_mapping.JsonType
    | type_mapping.CustomType(_) -> False
    type_mapping.DynamicType -> True
    type_mapping.ListType(inner) -> detect_dynamic_usage_in_gleam_type(inner)
    type_mapping.OptionType(inner) -> detect_dynamic_usage_in_gleam_type(inner)
  }
}

/// Detect if Option types are used in any field
fn detect_option_usage(fields: List(#(String, schema.TypeRef))) -> Bool {
  fields
  |> list.fold(False, fn(acc, field) {
    let #(_field_name, type_ref) = field

    // Convert to GleamType (use OutputContext for response types)
    case
      type_mapping.graphql_to_gleam_nullable(
        type_ref,
        type_mapping.OutputContext,
      )
    {
      Ok(gleam_type) -> {
        let needs_option = detect_option_usage_in_gleam_type(gleam_type)
        acc || needs_option
      }
      Error(_) -> acc
    }
  })
}

/// Detect if Dynamic types are used in any field
fn detect_dynamic_usage(fields: List(#(String, schema.TypeRef))) -> Bool {
  fields
  |> list.fold(False, fn(acc, field) {
    let #(_field_name, type_ref) = field

    // Convert to GleamType (use OutputContext for response types)
    case
      type_mapping.graphql_to_gleam_nullable(
        type_ref,
        type_mapping.OutputContext,
      )
    {
      Ok(gleam_type) -> {
        let needs_dynamic = detect_dynamic_usage_in_gleam_type(gleam_type)
        acc || needs_dynamic
      }
      Error(_) -> acc
    }
  })
}

/// Detect if Some/None constructors are needed (only for input serializers with optional fields)
fn detect_optional_input_fields(input_types: List(InputTypeInfo)) -> Bool {
  input_types
  |> list.any(fn(input_info) {
    input_info.input_fields
    |> list.any(fn(field) {
      // Check if field is NOT NonNullType (i.e., it's optional)
      case field.type_ref {
        schema.NonNullType(_) -> False
        _ -> True
      }
    })
  })
}

/// Generate imports section with conditional Option and Dynamic imports
fn imports_doc(
  needs_option: Bool,
  needs_dynamic: Bool,
  needs_option_constructors: Bool,
  needs_list: Bool,
) -> Document {
  // Minimal imports - just what we need for decoders and the squall client
  let core_imports = [
    "import gleam/dynamic/decode",
    "import gleam/http/request.{type Request}",
    "import gleam/json",
    "import squall",
  ]

  let optional_imports = case needs_option, needs_option_constructors {
    False, _ -> []
    True, False -> ["import gleam/option.{type Option}"]
    True, True -> ["import gleam/option.{type Option, Some, None}"]
  }

  let dynamic_imports = case needs_dynamic {
    True -> ["import gleam/dynamic.{type Dynamic}"]
    False -> []
  }

  let list_imports = case needs_list {
    True -> ["import gleam/list"]
    False -> []
  }

  list.append(core_imports, optional_imports)
  |> list.append(dynamic_imports)
  |> list.append(list_imports)
  |> list.map(doc.from_string)
  |> doc.join(with: doc.line)
}

// --- CODE GENERATION ---------------------------------------------------------

// Generate code for an operation
pub fn generate_operation(
  operation_name: String,
  source: String,
  operation: graphql_ast.Operation,
  schema_data: schema.Schema,
  _graphql_endpoint: String,
) -> Result(String, Error) {
  generate_operation_with_fragments(
    operation_name,
    source,
    operation,
    [],
    schema_data,
    "",
  )
}

pub fn generate_operation_with_fragments(
  operation_name: String,
  source: String,
  operation: graphql_ast.Operation,
  fragments: List(graphql_ast.Operation),
  schema_data: schema.Schema,
  _graphql_endpoint: String,
) -> Result(String, Error) {
  // Note: typename injection is disabled for the generate command
  // It's only enabled for unstable-cache command which needs it for cache normalization
  let modified_source = source

  // Extract selections and expand any fragments
  let selections = graphql_ast.get_selections(operation)

  use expanded_selections <- result.try(graphql_ast.expand_fragments(
    selections,
    fragments,
  ))

  // Determine root type based on operation type
  let root_type_name = case graphql_ast.get_operation_type(operation) {
    graphql_ast.Query -> schema_data.query_type
    graphql_ast.Mutation -> schema_data.mutation_type
    graphql_ast.Subscription -> schema_data.subscription_type
  }

  use root_type_name_str <- result.try(
    root_type_name
    |> option.to_result(error.InvalidSchemaResponse(
      "No root type for operation",
    )),
  )

  use root_type <- result.try(
    dict.get(schema_data.types, root_type_name_str)
    |> result.map_error(fn(_) {
      error.InvalidSchemaResponse("Root type not found: " <> root_type_name_str)
    }),
  )

  // Generate response type
  let response_type_name = to_pascal_case(operation_name) <> "Response"

  // Build field types from expanded selections
  use field_types <- result.try(collect_field_types(
    expanded_selections,
    root_type,
  ))

  // Collect nested types that need to be generated
  use nested_types <- result.try(collect_nested_types(
    expanded_selections,
    root_type,
    schema_data,
  ))

  // Generate nested type definitions and decoders
  let nested_docs =
    nested_types
    |> list.map(fn(nested_info) {
      let type_doc =
        generate_type_definition(nested_info.type_name, nested_info.fields)
      let decoder_doc =
        generate_decoder_with_schema(
          nested_info.type_name,
          nested_info.fields,
          nested_info.field_types,
        )
      [type_doc, decoder_doc]
    })
    |> list.flatten

  // Generate response type definition
  let type_def = generate_type_definition(response_type_name, field_types)

  // Generate response decoder
  let decoder =
    generate_decoder_with_schema(
      response_type_name,
      field_types,
      schema_data.types,
    )

  // Generate nested type serializers
  let nested_serializer_docs =
    nested_types
    |> list.map(fn(nested_info) {
      generate_response_serializer(
        nested_info.type_name,
        nested_info.fields,
        nested_info.field_types,
      )
    })

  // Generate response serializer
  let response_serializer =
    generate_response_serializer(
      response_type_name,
      field_types,
      schema_data.types,
    )

  // Collect Input types from variables
  let variables = graphql_ast.get_variables(operation)
  use input_types <- result.try(collect_input_types(
    variables,
    schema_data.types,
  ))

  // Generate Input type definitions and serializers
  let input_docs =
    input_types
    |> list.map(fn(input_info) {
      let type_doc = generate_input_type_definition(input_info)
      let serializer_doc = generate_input_serializer(input_info)
      [type_doc, serializer_doc]
    })
    |> list.flatten

  // Collect Enum types from variables and response fields
  use enum_types <- result.try(collect_enum_types(
    variables,
    field_types,
    schema_data.types,
  ))

  // Generate Enum type definitions, to_string functions, and decoders
  let enum_docs =
    enum_types
    |> list.map(fn(enum_info) {
      let type_doc = generate_enum_type_definition(enum_info)
      let to_string_doc = generate_enum_to_string(enum_info)
      let decoder_doc = generate_enum_decoder(enum_info)
      [type_doc, to_string_doc, decoder_doc]
    })
    |> list.flatten

  // Use the modified source string with __typename injected
  let function_def =
    generate_function(
      operation_name,
      response_type_name,
      variables,
      modified_source,
      schema_data.types,
    )

  // Detect Option type usage from all field types
  // Separate output fields (response/nested) from input fields for proper context detection
  let output_field_types =
    list.flatten([
      field_types,
      list.flat_map(nested_types, fn(nt) { nt.fields }),
    ])

  let input_field_types =
    list.flat_map(input_types, fn(it) {
      it.input_fields
      |> list.map(fn(iv) { #(iv.name, iv.type_ref) })
    })

  let all_field_types = list.append(output_field_types, input_field_types)

  let needs_option = detect_option_usage(all_field_types)
  // Only check output fields for Dynamic usage (input fields use json.Json, not Dynamic)
  let needs_dynamic = detect_dynamic_usage(output_field_types)
  let needs_option_constructors = detect_optional_input_fields(input_types)
  // Need gleam/list import when there are input types (for list.filter_map in serializers)
  let needs_list = case input_types {
    [] -> False
    _ -> True
  }

  // Build imports
  let imports =
    imports_doc(
      needs_option,
      needs_dynamic,
      needs_option_constructors,
      needs_list,
    )

  // Combine all code using doc combinators
  // Order: imports, enum types, input types, nested types, nested serializers, response type, response decoder, response serializer, function
  let all_docs =
    [imports, ..enum_docs]
    |> list.append(input_docs)
    |> list.append(nested_docs)
    |> list.append(nested_serializer_docs)
    |> list.append([type_def, decoder, response_serializer, function_def])
  let code =
    all_docs
    |> doc.join(with: doc.lines(2))
    |> doc.append(doc.line)
    |> doc.to_string(80)

  Ok(code)
}

// Collect field types from selections
fn collect_field_types(
  selections: List(graphql_ast.Selection),
  parent_type: schema.Type,
) -> Result(List(#(String, schema.TypeRef)), Error) {
  selections
  |> list.filter(fn(selection) {
    case selection {
      parser.Field(_, _, _, _) -> True
      parser.FragmentSpread(_) | parser.InlineFragment(_, _) -> False
    }
  })
  |> list.try_map(fn(selection) {
    case selection {
      parser.Field(field_name, _alias, _args, _nested) -> {
        // Handle special introspection fields
        case field_name {
          "__typename" -> {
            // __typename is a special meta-field that returns String!
            Ok(#(field_name, schema.NamedType("String", schema.Scalar)))
          }
          _ -> {
            // Find field in parent type
            let fields = schema.get_type_fields(parent_type)
            let field_result =
              fields
              |> list.find(fn(f) { f.name == field_name })

            field_result
            |> result.map(fn(field) { #(field_name, field.type_ref) })
            |> result.map_error(fn(_) {
              error.InvalidSchemaResponse("Field not found: " <> field_name)
            })
          }
        }
      }
      _ ->
        Error(error.InvalidGraphQLSyntax(
          "collect_field_types",
          0,
          "Unexpected selection type",
        ))
    }
  })
}

// Collect nested types that need to be generated
fn collect_nested_types(
  selections: List(graphql_ast.Selection),
  parent_type: schema.Type,
  schema_data: schema.Schema,
) -> Result(List(NestedTypeInfo), Error) {
  selections
  |> list.try_map(fn(selection) {
    case selection {
      parser.Field(field_name, _alias, _args, nested_selections) -> {
        // Handle special introspection fields
        case field_name {
          "__typename" -> {
            // __typename is a special meta-field that has no nested selections
            Ok([])
          }
          _ -> {
            // Find field in parent type
            let fields = schema.get_type_fields(parent_type)
            use field <- result.try(
              fields
              |> list.find(fn(f) { f.name == field_name })
              |> result.map_error(fn(_) {
                error.InvalidSchemaResponse("Field not found: " <> field_name)
              }),
            )

            // Check if this field has nested selections
            case nested_selections {
              [] -> Ok([])
              _ -> {
                // Get the type name from the field's type reference
                let type_name = get_base_type_name(field.type_ref)

                // Look up the type in schema
                use field_type <- result.try(
                  dict.get(schema_data.types, type_name)
                  |> result.map_error(fn(_) {
                    error.InvalidSchemaResponse("Type not found: " <> type_name)
                  }),
                )

                // Collect field types for this nested object
                use nested_field_types <- result.try(collect_field_types(
                  nested_selections,
                  field_type,
                ))

                // Recursively collect any deeper nested types
                use deeper_nested <- result.try(collect_nested_types(
                  nested_selections,
                  field_type,
                  schema_data,
                ))

                let nested_info =
                  NestedTypeInfo(
                    type_name: type_name,
                    fields: nested_field_types,
                    field_types: schema_data.types,
                  )

                Ok([nested_info, ..deeper_nested])
              }
            }
          }
        }
      }
      // Fragments not yet supported - ignore them
      parser.FragmentSpread(_) | parser.InlineFragment(_, _) -> Ok([])
    }
  })
  |> result.map(list.flatten)
}

// Extract the base type name from a TypeRef (unwrap NonNull and List)
fn get_base_type_name(type_ref: schema.TypeRef) -> String {
  case type_ref {
    schema.NamedType(name, _) -> name
    schema.NonNullType(inner) -> get_base_type_name(inner)
    schema.ListType(inner) -> get_base_type_name(inner)
  }
}

// Unwrap NonNull wrapper from optional type
fn unwrap_option_type(type_ref: schema.TypeRef) -> schema.TypeRef {
  case type_ref {
    schema.NonNullType(inner) -> inner
    _ -> type_ref
  }
}

// Collect all InputObject types used in variables
fn collect_input_types(
  variables: List(graphql_ast.Variable),
  schema_types: dict.Dict(String, schema.Type),
) -> Result(List(InputTypeInfo), Error) {
  variables
  |> list.try_map(fn(var) {
    let type_str = graphql_ast.get_variable_type_string(var)
    use schema_type_ref <- result.try(
      type_mapping.parse_type_string_with_schema(type_str, schema_types),
    )
    collect_input_types_from_type_ref(schema_type_ref, schema_types, [])
  })
  |> result.map(list.flatten)
  |> result.map(fn(input_types) {
    // Deduplicate by type name
    input_types
    |> list.fold(dict.new(), fn(acc, info) {
      dict.insert(acc, info.type_name, info)
    })
    |> dict.values
  })
}

// Recursively collect InputObject types from a type reference
fn collect_input_types_from_type_ref(
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
  collected: List(InputTypeInfo),
) -> Result(List(InputTypeInfo), Error) {
  case type_ref {
    schema.NamedType(name, kind) -> {
      case kind {
        schema.InputObject -> {
          // Check if we've already collected this type (avoid infinite recursion)
          let already_collected =
            list.any(collected, fn(info) { info.type_name == name })

          case already_collected {
            True -> Ok(collected)
            False -> {
              // Look up the InputObject type in schema
              use input_type <- result.try(
                dict.get(schema_types, name)
                |> result.map_error(fn(_) {
                  error.InvalidSchemaResponse(
                    "InputObject type not found: " <> name,
                  )
                }),
              )

              case input_type {
                schema.InputObjectType(_, input_fields, _) -> {
                  // Create InputTypeInfo
                  let info =
                    InputTypeInfo(
                      type_name: name,
                      input_fields: input_fields,
                      field_types: schema_types,
                    )

                  // Recursively collect nested InputObject types
                  use nested <- result.try(
                    input_fields
                    |> list.try_map(fn(field) {
                      collect_input_types_from_type_ref(
                        field.type_ref,
                        schema_types,
                        [info, ..collected],
                      )
                    })
                    |> result.map(list.flatten),
                  )

                  Ok([info, ..nested])
                }
                _ ->
                  Error(error.InvalidSchemaResponse(
                    "Expected InputObject type: " <> name,
                  ))
              }
            }
          }
        }
        _ -> Ok(collected)
      }
    }
    schema.NonNullType(inner) ->
      collect_input_types_from_type_ref(inner, schema_types, collected)
    schema.ListType(inner) ->
      collect_input_types_from_type_ref(inner, schema_types, collected)
  }
}

// Collect all Enum types used in variables and response fields
fn collect_enum_types(
  variables: List(graphql_ast.Variable),
  field_types: List(#(String, schema.TypeRef)),
  schema_types: dict.Dict(String, schema.Type),
) -> Result(List(EnumTypeInfo), Error) {
  // Collect from variables
  let var_enums =
    variables
    |> list.try_map(fn(var) {
      let type_str = graphql_ast.get_variable_type_string(var)
      use schema_type_ref <- result.try(
        type_mapping.parse_type_string_with_schema(type_str, schema_types),
      )
      collect_enum_types_from_type_ref(schema_type_ref, schema_types, [])
    })
    |> result.map(list.flatten)

  // Collect from response fields
  let field_enums =
    field_types
    |> list.map(fn(field) {
      let #(_name, type_ref) = field
      collect_enum_types_from_type_ref(type_ref, schema_types, [])
    })
    |> result.all
    |> result.map(list.flatten)

  use var_enum_list <- result.try(var_enums)
  use field_enum_list <- result.try(field_enums)

  // Combine and deduplicate
  Ok(
    list.append(var_enum_list, field_enum_list)
    |> list.fold(dict.new(), fn(acc, info) {
      dict.insert(acc, info.type_name, info)
    })
    |> dict.values,
  )
}

// Recursively collect Enum types from a type reference
fn collect_enum_types_from_type_ref(
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
  collected: List(EnumTypeInfo),
) -> Result(List(EnumTypeInfo), Error) {
  case type_ref {
    schema.NamedType(name, kind) -> {
      case kind {
        schema.Enum -> {
          // Check if we've already collected this enum
          let already_collected =
            list.any(collected, fn(info) { info.type_name == name })

          case already_collected {
            True -> Ok(collected)
            False -> {
              // Look up the Enum type in schema
              use enum_type <- result.try(
                dict.get(schema_types, name)
                |> result.map_error(fn(_) {
                  error.InvalidSchemaResponse("Enum type not found: " <> name)
                }),
              )

              case enum_type {
                schema.EnumType(_, enum_values, _) -> {
                  let info =
                    EnumTypeInfo(type_name: name, enum_values: enum_values)
                  Ok([info, ..collected])
                }
                _ ->
                  Error(error.InvalidSchemaResponse(
                    "Expected Enum type: " <> name,
                  ))
              }
            }
          }
        }
        _ -> Ok(collected)
      }
    }
    schema.NonNullType(inner) ->
      collect_enum_types_from_type_ref(inner, schema_types, collected)
    schema.ListType(inner) ->
      collect_enum_types_from_type_ref(inner, schema_types, collected)
  }
}

// Generate type definition
fn generate_type_definition(
  type_name: String,
  fields: List(#(String, schema.TypeRef)),
) -> Document {
  let field_docs =
    fields
    |> list.map(fn(field) {
      let #(name, type_ref) = field
      let sanitized_name = sanitize_field_name(name)
      use gleam_type <- result.try(type_mapping.graphql_to_gleam_nullable(
        type_ref,
        type_mapping.OutputContext,
      ))
      let field_doc =
        doc.concat([
          doc.from_string(sanitized_name <> ": "),
          doc.from_string(type_mapping.to_gleam_type_string(gleam_type)),
        ])
      Ok(field_doc)
    })
    |> list.filter_map(fn(r) { r })

  [
    doc.from_string("pub type " <> type_name <> " {"),
    [
      doc.line,
      call_doc(type_name, field_docs),
    ]
      |> doc.concat
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("}"),
  ]
  |> doc.concat
  |> doc.group
}

// Generate decoder with schema knowledge for nested types
fn generate_decoder_with_schema(
  type_name: String,
  fields: List(#(String, schema.TypeRef)),
  schema_types: dict.Dict(String, schema.Type),
) -> Document {
  let decoder_name = snake_case(type_name) <> "_decoder"

  let field_decoder_docs =
    fields
    |> list.map(fn(field) {
      let #(name, type_ref) = field
      let sanitized_name = sanitize_field_name(name)
      use gleam_type <- result.try(type_mapping.graphql_to_gleam_nullable(
        type_ref,
        type_mapping.OutputContext,
      ))
      let field_decoder =
        doc.concat([
          doc.from_string("use " <> sanitized_name <> " <- decode.field("),
          string_doc(name),
          doc.from_string(", "),
          doc.from_string(generate_field_decoder_with_schema(
            gleam_type,
            type_ref,
            schema_types,
          )),
          doc.from_string(")"),
        ])
      Ok(field_decoder)
    })
    |> list.filter_map(fn(r) { r })

  let constructor_args =
    fields
    |> list.map(fn(f) {
      let sanitized = sanitize_field_name(f.0)
      doc.from_string(sanitized <> ": " <> sanitized)
    })

  let success_line =
    doc.concat([
      doc.from_string("decode.success("),
      call_doc(type_name, constructor_args),
      doc.from_string(")"),
    ])

  let decoder_body = list.append(field_decoder_docs, [success_line])

  doc.concat([
    doc.from_string("pub fn " <> decoder_name <> "() -> decode.Decoder("),
    doc.from_string(type_name <> ") "),
    block(decoder_body),
  ])
}

// Generate field decoder string with schema knowledge
fn generate_field_decoder_with_schema(
  gleam_type: type_mapping.GleamType,
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
) -> String {
  case gleam_type {
    type_mapping.StringType -> "decode.string"
    type_mapping.IntType -> "decode.int"
    type_mapping.FloatType -> "decode.float"
    type_mapping.BoolType -> "decode.bool"
    type_mapping.JsonType -> "decode.dynamic"
    type_mapping.DynamicType -> "decode.dynamic"
    type_mapping.ListType(inner) -> {
      let inner_decoder =
        generate_field_decoder_with_schema_inner(inner, type_ref, schema_types)
      "decode.list(" <> inner_decoder <> ")"
    }
    type_mapping.OptionType(inner) -> {
      let inner_decoder =
        generate_field_decoder_with_schema_inner(inner, type_ref, schema_types)
      "decode.optional(" <> inner_decoder <> ")"
    }
    type_mapping.CustomType(name) -> {
      // Check if this is an object or enum type that has a decoder
      case dict.get(schema_types, name) {
        Ok(schema.ObjectType(_, _, _)) -> snake_case(name) <> "_decoder()"
        Ok(schema.EnumType(_, _, _)) -> snake_case(name) <> "_decoder()"
        _ -> "decode.dynamic"
      }
    }
  }
}

// Helper to generate decoder for inner types (handles unwrapping NonNull/List from TypeRef)
fn generate_field_decoder_with_schema_inner(
  gleam_type: type_mapping.GleamType,
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
) -> String {
  let base_type_name = get_base_type_name(type_ref)

  case gleam_type {
    type_mapping.ListType(inner) -> {
      let inner_decoder =
        generate_field_decoder_with_schema_inner(inner, type_ref, schema_types)
      "decode.list(" <> inner_decoder <> ")"
    }
    type_mapping.OptionType(inner) -> {
      let inner_decoder =
        generate_field_decoder_with_schema_inner(inner, type_ref, schema_types)
      "decode.optional(" <> inner_decoder <> ")"
    }
    type_mapping.CustomType(_) ->
      case dict.get(schema_types, base_type_name) {
        Ok(schema.ObjectType(_, _, _)) ->
          snake_case(base_type_name) <> "_decoder()"
        Ok(schema.EnumType(_, _, _)) ->
          snake_case(base_type_name) <> "_decoder()"
        _ -> "decode.dynamic"
      }
    _ -> generate_field_decoder(gleam_type)
  }
}

// Generate field decoder string
fn generate_field_decoder(gleam_type: type_mapping.GleamType) -> String {
  case gleam_type {
    type_mapping.StringType -> "decode.string"
    type_mapping.IntType -> "decode.int"
    type_mapping.FloatType -> "decode.float"
    type_mapping.BoolType -> "decode.bool"
    type_mapping.JsonType -> "decode.dynamic"
    type_mapping.DynamicType -> "decode.dynamic"
    type_mapping.ListType(inner) ->
      "decode.list(" <> generate_field_decoder(inner) <> ")"
    type_mapping.OptionType(inner) ->
      "decode.optional(" <> generate_field_decoder(inner) <> ")"
    type_mapping.CustomType(_name) -> "decode.dynamic"
  }
}

// Generate Input type definition
fn generate_input_type_definition(input_info: InputTypeInfo) -> Document {
  let field_docs =
    input_info.input_fields
    |> list.map(fn(input_value) {
      let sanitized_name = sanitize_field_name(input_value.name)
      use gleam_type <- result.try(type_mapping.graphql_to_gleam_nullable(
        input_value.type_ref,
        type_mapping.InputContext,
      ))
      let field_doc =
        doc.concat([
          doc.from_string(sanitized_name <> ": "),
          doc.from_string(type_mapping.to_gleam_type_string(gleam_type)),
        ])
      Ok(field_doc)
    })
    |> list.filter_map(fn(r) { r })

  [
    doc.from_string("pub type " <> input_info.type_name <> " {"),
    [
      doc.line,
      call_doc(input_info.type_name, field_docs),
    ]
      |> doc.concat
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("}"),
  ]
  |> doc.concat
  |> doc.group
}

// Generate Input serializer function
fn generate_input_serializer(input_info: InputTypeInfo) -> Document {
  let serializer_name = snake_case(input_info.type_name) <> "_to_json"
  let param_name = "input"

  let field_entries =
    input_info.input_fields
    |> list.map(fn(input_value) {
      let sanitized_name = sanitize_field_name(input_value.name)
      use gleam_type <- result.try(type_mapping.graphql_to_gleam_nullable(
        input_value.type_ref,
        type_mapping.InputContext,
      ))

      // Generate code that wraps optional fields in case/Some/None
      case gleam_type {
        type_mapping.OptionType(inner) -> {
          let inner_encoder =
            encode_input_field_value(
              "val",
              inner,
              unwrap_option_type(input_value.type_ref),
              input_info.field_types,
            )

          Ok(
            doc.concat([
              doc.from_string("{"),
              doc.line,
              doc.from_string(
                "  case " <> param_name <> "." <> sanitized_name <> " {",
              ),
              doc.line,
              doc.from_string("    Some(val) -> Some(#("),
              string_doc(input_value.name),
              doc.from_string(", "),
              inner_encoder,
              doc.from_string("))"),
              doc.line,
              doc.from_string("    None -> None"),
              doc.line,
              doc.from_string("  }"),
              doc.line,
              doc.from_string("}"),
            ]),
          )
        }
        _ -> {
          // Non-optional field: always include
          let value_expr =
            encode_input_field_value(
              param_name <> "." <> sanitized_name,
              gleam_type,
              input_value.type_ref,
              input_info.field_types,
            )

          Ok(
            doc.concat([
              doc.from_string("Some(#("),
              string_doc(input_value.name),
              doc.from_string(", "),
              value_expr,
              doc.from_string("))"),
            ]),
          )
        }
      }
    })
    |> list.filter_map(fn(r) { r })

  let body =
    doc.concat([
      comma_list("[", field_entries, "]"),
      doc.line,
      doc.from_string("|> list.filter_map(fn(x) {"),
      doc.line,
      doc.from_string("  case x {"),
      doc.line,
      doc.from_string("    Some(val) -> Ok(val)"),
      doc.line,
      doc.from_string("    None -> Error(Nil)"),
      doc.line,
      doc.from_string("  }"),
      doc.line,
      doc.from_string("})"),
      doc.line,
      doc.from_string("|> json.object"),
    ])

  doc.concat([
    doc.from_string("fn " <> serializer_name <> "("),
    doc.from_string(param_name <> ": " <> input_info.type_name),
    doc.from_string(") -> json.Json "),
    block([body]),
  ])
}

// Encode a field value for Input serialization
fn encode_input_field_value(
  field_access: String,
  gleam_type: type_mapping.GleamType,
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
) -> Document {
  case gleam_type {
    type_mapping.StringType ->
      call_doc("json.string", [doc.from_string(field_access)])
    type_mapping.IntType ->
      call_doc("json.int", [doc.from_string(field_access)])
    type_mapping.FloatType ->
      call_doc("json.float", [doc.from_string(field_access)])
    type_mapping.BoolType ->
      call_doc("json.bool", [doc.from_string(field_access)])
    type_mapping.JsonType ->
      // JSON scalar: use the json.Json value directly without wrapping
      doc.from_string(field_access)
    type_mapping.DynamicType ->
      // This case handles other custom scalars that map to Dynamic
      doc.from_string(field_access)
    type_mapping.ListType(inner) -> {
      let base_type_name = get_base_type_name(type_ref)
      case dict.get(schema_types, base_type_name) {
        Ok(schema.InputObjectType(_, _, _)) -> {
          // List of InputObjects
          call_doc("json.array", [
            doc.from_string("from: " <> field_access),
            doc.from_string("of: " <> snake_case(base_type_name) <> "_to_json"),
          ])
        }
        _ -> {
          // List of scalars
          let of_fn = case inner {
            type_mapping.StringType -> "json.string"
            type_mapping.IntType -> "json.int"
            type_mapping.FloatType -> "json.float"
            type_mapping.BoolType -> "json.bool"
            type_mapping.JsonType -> "fn(x) { x }"
            _ -> "json.string"
          }
          call_doc("json.array", [
            doc.from_string("from: " <> field_access),
            doc.from_string("of: " <> of_fn),
          ])
        }
      }
    }
    type_mapping.OptionType(inner) -> {
      let base_type_name = get_base_type_name(type_ref)
      case dict.get(schema_types, base_type_name) {
        Ok(schema.InputObjectType(_, _, _)) -> {
          // Optional InputObject
          call_doc("json.nullable", [
            doc.from_string(field_access),
            doc.from_string(snake_case(base_type_name) <> "_to_json"),
          ])
        }
        _ -> {
          // Optional scalar or list
          let inner_encoder = case inner {
            type_mapping.StringType -> "json.string"
            type_mapping.IntType -> "json.int"
            type_mapping.FloatType -> "json.float"
            type_mapping.BoolType -> "json.bool"
            type_mapping.JsonType -> "fn(x) { x }"
            type_mapping.ListType(_) -> {
              // This is handled by recursion, but for now use a lambda
              let of_fn = case inner {
                type_mapping.ListType(type_mapping.StringType) -> "json.string"
                type_mapping.ListType(type_mapping.IntType) -> "json.int"
                type_mapping.ListType(type_mapping.FloatType) -> "json.float"
                type_mapping.ListType(type_mapping.BoolType) -> "json.bool"
                _ -> "json.string"
              }
              "fn(list) { json.array(from: list, of: " <> of_fn <> ") }"
            }
            _ -> "json.string"
          }
          call_doc("json.nullable", [
            doc.from_string(field_access),
            doc.from_string(inner_encoder),
          ])
        }
      }
    }
    type_mapping.CustomType(name) -> {
      // Check if this is an Enum or InputObject
      case dict.get(schema_types, name) {
        Ok(schema.EnumType(_, _, _)) ->
          // Enum: convert to string first, then wrap in json.string
          call_doc("json.string", [
            doc.from_string(
              snake_case(name) <> "_to_string(" <> field_access <> ")",
            ),
          ])
        _ ->
          // InputObject
          call_doc(snake_case(name) <> "_to_json", [
            doc.from_string(field_access),
          ])
      }
    }
  }
}

// Generate Enum type definition
fn generate_enum_type_definition(enum_info: EnumTypeInfo) -> Document {
  let variant_docs =
    enum_info.enum_values
    |> list.map(fn(value) { doc.from_string(to_pascal_case(value)) })

  [
    doc.from_string("pub type " <> enum_info.type_name <> " {"),
    [
      doc.line,
      doc.join(variant_docs, doc.line),
    ]
      |> doc.concat
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("}"),
  ]
  |> doc.concat
  |> doc.group
}

// Generate Enum to String conversion function
fn generate_enum_to_string(enum_info: EnumTypeInfo) -> Document {
  let function_name = snake_case(enum_info.type_name) <> "_to_string"

  let match_arms =
    enum_info.enum_values
    |> list.map(fn(value) {
      doc.concat([
        doc.from_string(to_pascal_case(value) <> " -> "),
        string_doc(value),
      ])
    })

  let case_expr =
    [
      doc.from_string("case value {"),
      [doc.line, doc.join(match_arms, doc.line)]
        |> doc.concat
        |> doc.nest(by: indent),
      doc.line,
      doc.from_string("}"),
    ]
    |> doc.concat

  doc.concat([
    doc.from_string(
      "pub fn "
      <> function_name
      <> "(value: "
      <> enum_info.type_name
      <> ") -> String ",
    ),
    block([case_expr]),
  ])
}

// Generate Enum decoder function
fn generate_enum_decoder(enum_info: EnumTypeInfo) -> Document {
  let decoder_name = snake_case(enum_info.type_name) <> "_decoder"

  let match_arms =
    enum_info.enum_values
    |> list.map(fn(value) {
      doc.concat([
        string_doc(value),
        doc.from_string(" -> decode.success(" <> to_pascal_case(value) <> ")"),
      ])
    })

  // Use the first enum variant as the zero value for failure
  let first_variant = case enum_info.enum_values {
    [first, ..] -> to_pascal_case(first)
    [] -> "UnknownVariant"
  }

  let all_match_arms =
    list.append(match_arms, [
      doc.concat([
        doc.from_string("_other -> decode.failure("),
        doc.from_string(first_variant),
        doc.from_string(", "),
        string_doc(enum_info.type_name),
        doc.from_string(")"),
      ]),
    ])

  let decoder_body = [
    doc.from_string("decode.string"),
    doc.line,
    doc.from_string("|> decode.then(fn(str) {"),
    [
      doc.line,
      doc.from_string("case str {"),
      [doc.line, doc.join(all_match_arms, doc.line)]
        |> doc.concat
        |> doc.nest(by: indent),
      doc.line,
      doc.from_string("}"),
    ]
      |> doc.concat
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("})"),
  ]

  doc.concat([
    doc.from_string(
      "pub fn "
      <> decoder_name
      <> "() -> decode.Decoder("
      <> enum_info.type_name
      <> ") ",
    ),
    block(decoder_body),
  ])
}

// Generate Response serializer function (for output types)
fn generate_response_serializer(
  type_name: String,
  fields: List(#(String, schema.TypeRef)),
  schema_types: dict.Dict(String, schema.Type),
) -> Document {
  let serializer_name = snake_case(type_name) <> "_to_json"
  let param_name = "input"

  let field_entries =
    fields
    |> list.map(fn(field) {
      let #(field_name, type_ref) = field
      let sanitized_name = sanitize_field_name(field_name)
      use gleam_type <- result.try(type_mapping.graphql_to_gleam_nullable(
        type_ref,
        type_mapping.OutputContext,
      ))

      // Generate encoder for the field
      let value_expr =
        encode_response_field_value(
          param_name <> "." <> sanitized_name,
          gleam_type,
          type_ref,
          schema_types,
        )

      Ok(
        doc.concat([
          doc.from_string("#("),
          string_doc(field_name),
          doc.from_string(", "),
          value_expr,
          doc.from_string(")"),
        ]),
      )
    })
    |> list.filter_map(fn(r) { r })

  let body = call_doc("json.object", [comma_list("[", field_entries, "]")])

  doc.concat([
    doc.from_string("pub fn " <> serializer_name <> "("),
    doc.from_string(param_name <> ": " <> type_name),
    doc.from_string(") -> json.Json "),
    block([body]),
  ])
}

// Encode a field value for Response serialization
fn encode_response_field_value(
  field_access: String,
  gleam_type: type_mapping.GleamType,
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
) -> Document {
  case gleam_type {
    type_mapping.StringType ->
      call_doc("json.string", [doc.from_string(field_access)])
    type_mapping.IntType ->
      call_doc("json.int", [doc.from_string(field_access)])
    type_mapping.FloatType ->
      call_doc("json.float", [doc.from_string(field_access)])
    type_mapping.BoolType ->
      call_doc("json.bool", [doc.from_string(field_access)])
    type_mapping.JsonType ->
      // JSON scalar: use the json.Json value directly without wrapping
      doc.from_string(field_access)
    type_mapping.DynamicType ->
      // Dynamic types should be serialized as-is
      doc.from_string(field_access)
    type_mapping.ListType(inner) -> {
      let base_type_name = get_base_type_name(type_ref)
      case dict.get(schema_types, base_type_name) {
        Ok(schema.ObjectType(_, _, _)) -> {
          // List of Objects
          call_doc("json.array", [
            doc.from_string("from: " <> field_access),
            doc.from_string("of: " <> snake_case(base_type_name) <> "_to_json"),
          ])
        }
        Ok(schema.EnumType(_, _, _)) -> {
          // List of Enums
          call_doc("json.array", [
            doc.from_string("from: " <> field_access),
            doc.from_string(
              "of: fn(v) { json.string("
              <> snake_case(base_type_name)
              <> "_to_string(v)) }",
            ),
          ])
        }
        _ -> {
          // List of scalars
          let of_fn = case inner {
            type_mapping.StringType -> "json.string"
            type_mapping.IntType -> "json.int"
            type_mapping.FloatType -> "json.float"
            type_mapping.BoolType -> "json.bool"
            type_mapping.JsonType -> "fn(x) { x }"
            _ -> "json.string"
          }
          call_doc("json.array", [
            doc.from_string("from: " <> field_access),
            doc.from_string("of: " <> of_fn),
          ])
        }
      }
    }
    type_mapping.OptionType(inner) -> {
      // Check if inner is a list first, before checking for objects
      case inner {
        type_mapping.ListType(_) -> {
          // Optional list - need to generate a lambda that handles the array
          let of_fn = case inner {
            type_mapping.ListType(type_mapping.StringType) -> "json.string"
            type_mapping.ListType(type_mapping.IntType) -> "json.int"
            type_mapping.ListType(type_mapping.FloatType) -> "json.float"
            type_mapping.ListType(type_mapping.BoolType) -> "json.bool"
            type_mapping.ListType(type_mapping.JsonType) -> "fn(x) { x }"
            type_mapping.ListType(inner_inner) -> {
              case inner_inner {
                type_mapping.CustomType(name) ->
                  case dict.get(schema_types, name) {
                    Ok(schema.ObjectType(_, _, _)) ->
                      snake_case(name) <> "_to_json"
                    Ok(schema.EnumType(_, _, _)) ->
                      "fn(v) { json.string("
                      <> snake_case(name)
                      <> "_to_string(v)) }"
                    _ -> "json.string"
                  }
                _ -> "json.string"
              }
            }
          }
          let inner_encoder =
            "fn(list) { json.array(from: list, of: " <> of_fn <> ") }"
          call_doc("json.nullable", [
            doc.from_string(field_access),
            doc.from_string(inner_encoder),
          ])
        }
        _ -> {
          // Not a list, check if it's an object, enum, or scalar
          let base_type_name = get_base_type_name(type_ref)
          case dict.get(schema_types, base_type_name) {
            Ok(schema.ObjectType(_, _, _)) -> {
              // Optional Object
              call_doc("json.nullable", [
                doc.from_string(field_access),
                doc.from_string(snake_case(base_type_name) <> "_to_json"),
              ])
            }
            Ok(schema.EnumType(_, _, _)) -> {
              // Optional Enum
              call_doc("json.nullable", [
                doc.from_string(field_access),
                doc.from_string(
                  "fn(v) { json.string("
                  <> snake_case(base_type_name)
                  <> "_to_string(v)) }",
                ),
              ])
            }
            _ -> {
              // Optional scalar
              let inner_encoder = case inner {
                type_mapping.StringType -> "json.string"
                type_mapping.IntType -> "json.int"
                type_mapping.FloatType -> "json.float"
                type_mapping.BoolType -> "json.bool"
                type_mapping.JsonType -> "fn(x) { x }"
                _ -> "json.string"
              }
              call_doc("json.nullable", [
                doc.from_string(field_access),
                doc.from_string(inner_encoder),
              ])
            }
          }
        }
      }
    }
    type_mapping.CustomType(name) -> {
      // Check if this is an Object, Enum, or other custom type
      case dict.get(schema_types, name) {
        Ok(schema.ObjectType(_, _, _)) ->
          call_doc(snake_case(name) <> "_to_json", [
            doc.from_string(field_access),
          ])
        Ok(schema.EnumType(_, _, _)) ->
          // Enum: convert to string first, then wrap in json.string
          call_doc("json.string", [
            doc.from_string(
              snake_case(name) <> "_to_string(" <> field_access <> ")",
            ),
          ])
        _ ->
          // Fallback to string if not an object or enum type
          call_doc("json.string", [doc.from_string(field_access)])
      }
    }
  }
}

// Generate two functions: one to prepare the request, one to parse the response
fn generate_function(
  operation_name: String,
  response_type_name: String,
  variables: List(graphql_ast.Variable),
  query_string: String,
  schema_types: dict.Dict(String, schema.Type),
) -> Document {
  // Generate the prepare request function
  let prepare_function =
    generate_prepare_function(
      operation_name,
      variables,
      query_string,
      schema_types,
    )

  // Generate the parse response function
  let parse_function =
    generate_parse_function(operation_name, response_type_name)

  // Combine both functions with a line break
  doc.concat([prepare_function, doc.lines(2), parse_function])
}

// Generate the function that prepares the HTTP request
fn generate_prepare_function(
  operation_name: String,
  variables: List(graphql_ast.Variable),
  query_string: String,
  schema_types: dict.Dict(String, schema.Type),
) -> Document {
  let function_name = operation_name

  // Build parameter list as documents
  let param_docs = case variables {
    [] -> [doc.from_string("client: squall.Client")]
    vars -> {
      let var_param_docs =
        vars
        |> list.map(fn(var) {
          let type_str = graphql_ast.get_variable_type_string(var)
          use schema_type_ref <- result.try(
            type_mapping.parse_type_string_with_schema(type_str, schema_types),
          )
          use gleam_type <- result.try(type_mapping.graphql_to_gleam(
            schema_type_ref,
            type_mapping.InputContext,
          ))
          let param_name = snake_case(graphql_ast.get_variable_name(var))
          Ok(doc.from_string(
            param_name <> ": " <> type_mapping.to_gleam_type_string(gleam_type),
          ))
        })
        |> list.filter_map(fn(r) { r })

      [doc.from_string("client: squall.Client"), ..var_param_docs]
    }
  }

  // Build variables JSON object construction code
  let variables_code = case variables {
    [] -> call_doc("json.object", [doc.from_string("[]")])
    vars -> {
      let var_entry_docs =
        vars
        |> list.map(fn(var) {
          let type_str = graphql_ast.get_variable_type_string(var)
          use schema_type_ref <- result.try(
            type_mapping.parse_type_string_with_schema(type_str, schema_types),
          )
          use gleam_type <- result.try(type_mapping.graphql_to_gleam(
            schema_type_ref,
            type_mapping.InputContext,
          ))
          let param_name = snake_case(graphql_ast.get_variable_name(var))
          let value_encoder =
            encode_variable_value(
              param_name,
              gleam_type,
              schema_type_ref,
              schema_types,
            )
          Ok(
            doc.concat([
              doc.from_string("#("),
              string_doc(var.name),
              doc.from_string(", "),
              value_encoder,
              doc.from_string(")"),
            ]),
          )
        })
        |> list.filter_map(fn(r) { r })

      call_doc("json.object", [comma_list("[", var_entry_docs, "]")])
    }
  }

  // Build function body that calls squall.prepare_request
  let body_doc =
    call_doc("squall.prepare_request", [
      doc.from_string("client"),
      string_doc(query_string),
      variables_code,
    ])

  // Build function signature with explicit return type
  doc.concat([
    doc.from_string("pub fn " <> function_name),
    comma_list("(", param_docs, ")"),
    doc.from_string(" -> Result(Request(String), String) "),
    block([body_doc]),
  ])
}

// Generate the function that parses the response body
fn generate_parse_function(
  operation_name: String,
  response_type_name: String,
) -> Document {
  let function_name = "parse_" <> operation_name <> "_response"
  let decoder_name = snake_case(response_type_name) <> "_decoder"

  let body_doc =
    call_doc("squall.parse_response", [
      doc.from_string("body"),
      doc.from_string(decoder_name <> "()"),
    ])

  doc.concat([
    doc.from_string("pub fn " <> function_name),
    doc.from_string("(body: String) -> Result("),
    doc.from_string(response_type_name),
    doc.from_string(", String) "),
    block([body_doc]),
  ])
}

fn encode_variable_value(
  var_name: String,
  gleam_type: type_mapping.GleamType,
  type_ref: schema.TypeRef,
  schema_types: dict.Dict(String, schema.Type),
) -> Document {
  case gleam_type {
    type_mapping.StringType ->
      call_doc("json.string", [doc.from_string(var_name)])
    type_mapping.IntType -> call_doc("json.int", [doc.from_string(var_name)])
    type_mapping.FloatType ->
      call_doc("json.float", [doc.from_string(var_name)])
    type_mapping.BoolType -> call_doc("json.bool", [doc.from_string(var_name)])
    type_mapping.JsonType ->
      // JSON scalar: use the json.Json value directly without wrapping
      doc.from_string(var_name)
    type_mapping.DynamicType ->
      // This case handles other custom scalars that map to Dynamic
      doc.from_string(var_name)
    type_mapping.ListType(inner) -> {
      let base_type_name = get_base_type_name(type_ref)
      case dict.get(schema_types, base_type_name) {
        Ok(schema.InputObjectType(_, _, _)) -> {
          // List of InputObjects
          call_doc("json.array", [
            doc.from_string("from: " <> var_name),
            doc.from_string("of: " <> snake_case(base_type_name) <> "_to_json"),
          ])
        }
        _ -> {
          // List of scalars
          let encoder = case inner {
            type_mapping.StringType -> "json.string"
            type_mapping.IntType -> "json.int"
            type_mapping.FloatType -> "json.float"
            type_mapping.BoolType -> "json.bool"
            type_mapping.JsonType -> "fn(x) { x }"
            _ -> "json.string"
          }
          call_doc("json.array", [
            doc.from_string("from: " <> var_name),
            doc.from_string("of: " <> encoder),
          ])
        }
      }
    }
    type_mapping.OptionType(inner) -> {
      let base_type_name = get_base_type_name(type_ref)
      case dict.get(schema_types, base_type_name) {
        Ok(schema.InputObjectType(_, _, _)) -> {
          // Optional InputObject
          call_doc("json.nullable", [
            doc.from_string(var_name),
            doc.from_string(snake_case(base_type_name) <> "_to_json"),
          ])
        }
        _ -> {
          // Optional scalar or list - need to unwrap the type_ref
          let inner_type_ref = case type_ref {
            schema.NonNullType(t) -> t
            t -> t
          }
          call_doc("json.nullable", [
            doc.from_string(var_name),
            encode_variable_value("value", inner, inner_type_ref, schema_types),
          ])
        }
      }
    }
    type_mapping.CustomType(name) -> {
      // Check if this is an Enum or InputObject
      case dict.get(schema_types, name) {
        Ok(schema.EnumType(_, _, _)) ->
          // Enum: convert to string first, then wrap in json.string
          call_doc("json.string", [
            doc.from_string(
              snake_case(name) <> "_to_string(" <> var_name <> ")",
            ),
          ])
        Ok(schema.InputObjectType(_, _, _)) ->
          call_doc(snake_case(name) <> "_to_json", [doc.from_string(var_name)])
        _ -> call_doc("json.string", [doc.from_string(var_name)])
      }
    }
  }
}

// Helper functions

fn capitalize(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}

fn to_pascal_case(s: String) -> String {
  s
  |> string.split("_")
  |> list.map(capitalize)
  |> string.join("")
}

fn snake_case(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.fold("", fn(acc, char) {
    case string.uppercase(char) == char && char != string.lowercase(char) {
      True ->
        case acc {
          "" -> string.lowercase(char)
          _ -> acc <> "_" <> string.lowercase(char)
        }
      False -> acc <> char
    }
  })
}
