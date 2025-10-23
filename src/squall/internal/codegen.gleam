import glam/doc.{type Document}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import squall/internal/error.{type Error}
import squall/internal/parser
import squall/internal/schema
import squall/internal/type_mapping

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

/// A pretty printed let assignment.
fn let_var(name: String, body: Document) -> Document {
  doc.group(doc.concat([
    doc.from_string("let " <> name <> " ="),
    doc.nest(doc.concat([doc.line, body]), by: indent),
  ]))
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
  let snake_cased = snake_case(name)
  case list.contains(reserved_keywords, snake_cased) {
    True -> snake_cased <> "_"
    False -> snake_cased
  }
}

/// Generate imports section
fn imports_doc() -> Document {
  let import_lines = [
    "import gleam/dynamic/decode",
    "import gleam/http",
    "import gleam/http/request",
    "import gleam/httpc",
    "import gleam/json",
    "import gleam/list",
    "import gleam/option.{type Option}",
    "import gleam/result",
    "import squall",
  ]

  import_lines
  |> list.map(doc.from_string)
  |> doc.join(with: doc.line)
}

// --- CODE GENERATION ---------------------------------------------------------

// Generate code for an operation
pub fn generate_operation(
  operation_name: String,
  operation: parser.Operation,
  schema_data: schema.Schema,
  _graphql_endpoint: String,
) -> Result(String, Error) {
  // Extract selections
  let selections = parser.get_selections(operation)

  // Determine root type based on operation type
  let root_type_name = case parser.get_operation_type(operation) {
    parser.Query -> schema_data.query_type
    parser.Mutation -> schema_data.mutation_type
    parser.Subscription -> schema_data.subscription_type
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

  // Build field types from selections
  use field_types <- result.try(collect_field_types(selections, root_type))

  // Collect nested types that need to be generated
  use nested_types <- result.try(collect_nested_types(
    selections,
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
    generate_decoder_with_schema(response_type_name, field_types, schema_data.types)

  // Collect Input types from variables
  let variables = parser.get_variables(operation)
  use input_types <- result.try(collect_input_types(variables, schema_data.types))

  // Generate Input type definitions and serializers
  let input_docs =
    input_types
    |> list.map(fn(input_info) {
      let type_doc = generate_input_type_definition(input_info)
      let serializer_doc = generate_input_serializer(input_info)
      [type_doc, serializer_doc]
    })
    |> list.flatten

  // Generate function
  let function_def =
    generate_function(
      operation_name,
      response_type_name,
      variables,
      build_query_string(operation),
      schema_data.types,
    )

  // Build imports
  let imports = imports_doc()

  // Combine all code using doc combinators
  // Order: imports, input types, nested types, response type, response decoder, function
  let all_docs =
    [imports, ..input_docs]
    |> list.append(nested_docs)
    |> list.append([type_def, decoder, function_def])
  let code =
    all_docs
    |> doc.join(with: doc.lines(2))
    |> doc.append(doc.line)
    |> doc.to_string(80)

  Ok(code)
}

// Collect field types from selections
fn collect_field_types(
  selections: List(parser.Selection),
  parent_type: schema.Type,
) -> Result(List(#(String, schema.TypeRef)), Error) {
  selections
  |> list.try_map(fn(selection) {
    case selection {
      parser.FieldSelection(field_name, _alias, _args, _nested) -> {
        // Find field in parent type
        let fields = schema.get_type_fields(parent_type)
        let field_result =
          fields
          |> list.find(fn(f) { f.name == field_name })

        use field <- result.try(
          field_result
          |> result.map_error(fn(_) {
            error.InvalidSchemaResponse("Field not found: " <> field_name)
          }),
        )

        Ok(#(field_name, field.type_ref))
      }
    }
  })
}

// Collect nested types that need to be generated
fn collect_nested_types(
  selections: List(parser.Selection),
  parent_type: schema.Type,
  schema_data: schema.Schema,
) -> Result(List(NestedTypeInfo), Error) {
  selections
  |> list.try_map(fn(selection) {
    case selection {
      parser.FieldSelection(field_name, _alias, _args, nested_selections) -> {
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

// Collect all InputObject types used in variables
fn collect_input_types(
  variables: List(parser.Variable),
  schema_types: dict.Dict(String, schema.Type),
) -> Result(List(InputTypeInfo), Error) {
  variables
  |> list.try_map(fn(var) {
    use schema_type_ref <- result.try(
      type_mapping.parser_type_to_schema_type_with_schema(
        var.type_ref,
        schema_types,
      ),
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
                  error.InvalidSchemaResponse("InputObject type not found: " <> name)
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
      // Check if this is an object type that has a decoder
      case dict.get(schema_types, name) {
        Ok(schema.ObjectType(_, _, _)) ->
          snake_case(name) <> "_decoder()"
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
      ))

      let value_expr =
        encode_input_field_value(
          param_name <> "." <> sanitized_name,
          gleam_type,
          input_value.type_ref,
          input_info.field_types,
        )

      Ok(
        doc.concat([
          doc.from_string("#("),
          string_doc(input_value.name),
          doc.from_string(", "),
          value_expr,
          doc.from_string(")"),
        ]),
      )
    })
    |> list.filter_map(fn(r) { r })

  let body =
    call_doc("json.object", [comma_list("[", field_entries, "]")])

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
      // This is an InputObject
      call_doc(snake_case(name) <> "_to_json", [doc.from_string(field_access)])
    }
  }
}

// Generate function
fn generate_function(
  operation_name: String,
  response_type_name: String,
  variables: List(parser.Variable),
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
          use schema_type_ref <- result.try(
            type_mapping.parser_type_to_schema_type_with_schema(
              var.type_ref,
              schema_types,
            ),
          )
          use gleam_type <- result.try(type_mapping.graphql_to_gleam(
            schema_type_ref,
          ))
          let param_name = snake_case(var.name)
          Ok(
            doc.from_string(
              param_name <> ": " <> type_mapping.to_gleam_type_string(gleam_type),
            ),
          )
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
          use schema_type_ref <- result.try(
            type_mapping.parser_type_to_schema_type_with_schema(
              var.type_ref,
              schema_types,
            ),
          )
          use gleam_type <- result.try(type_mapping.graphql_to_gleam(
            schema_type_ref,
          ))
          let param_name = snake_case(var.name)
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

  // Build function body
  let body_docs = [
    let_var("query", string_doc(query_string)),
    let_var("variables", variables_code),
    let_var(
      "body",
      call_doc("json.object", [
        comma_list("[", [
          doc.from_string("#(\"query\", json.string(query))"),
          doc.from_string("#(\"variables\", variables)"),
        ], "]"),
      ]),
    ),
    doc.concat([
      doc.from_string("use req <- result.try("),
      doc.concat([
        doc.line,
        call_doc("request.to", [doc.from_string("client.endpoint")]),
        doc.line,
        doc.from_string("|> result.map_error(fn(_) { \"Invalid endpoint URL\" }),"),
      ])
        |> doc.nest(by: indent),
      doc.line,
      doc.from_string(")"),
    ]),
    let_var(
      "req",
      doc.concat([
        doc.from_string("req"),
        doc.line,
        doc.from_string("|> request.set_method(http.Post)"),
        doc.line,
        doc.from_string("|> request.set_body(json.to_string(body))"),
        doc.line,
        doc.from_string("|> request.set_header(\"content-type\", \"application/json\")"),
      ]),
    ),
    let_var(
      "req",
      doc.concat([
        doc.from_string("list.fold(client.headers, req, fn(r, header) {"),
        doc.line,
        doc.from_string("  request.set_header(r, header.0, header.1)"),
        doc.line,
        doc.from_string("})"),
      ]),
    ),
    doc.concat([
      doc.from_string("use resp <- result.try("),
      doc.concat([
        doc.line,
        call_doc("httpc.send", [doc.from_string("req")]),
        doc.line,
        doc.from_string("|> result.map_error(fn(_) { \"HTTP request failed\" }),"),
      ])
        |> doc.nest(by: indent),
      doc.line,
      doc.from_string(")"),
    ]),
    doc.concat([
      doc.from_string("use json_value <- result.try("),
      doc.concat([
        doc.line,
        call_doc("json.parse", [
          doc.from_string("from: resp.body"),
          doc.from_string("using: decode.dynamic"),
        ]),
        doc.line,
        doc.from_string(
          "|> result.map_error(fn(_) { \"Failed to decode JSON response\" }),",
        ),
      ])
        |> doc.nest(by: indent),
      doc.line,
      doc.from_string(")"),
    ]),
    doc.concat([
      doc.from_string("let data_and_response_decoder = {"),
      doc.line |> doc.nest(by: indent),
      doc.concat([
        doc.from_string("use data <- decode.field(\"data\", " <> snake_case(response_type_name) <> "_decoder())"),
        doc.line,
        doc.from_string("decode.success(data)"),
      ]) |> doc.nest(by: indent),
      doc.line,
      doc.from_string("}"),
    ]),
    doc.concat([
      doc.from_string("decode.run(json_value, data_and_response_decoder)"),
      doc.line,
      doc.from_string("|> result.map_error(fn(_) { \"Failed to decode response data\" })"),
    ]),
  ]

  // Build function signature
  // Don't use call_doc for return type to prevent it from breaking before parameters
  let return_type = doc.from_string(
    "Result(" <> response_type_name <> ", String)",
  )

  doc.concat([
    doc.from_string("pub fn " <> function_name),
    comma_list("(", param_docs, ")"),
    doc.from_string(" -> "),
    return_type,
    doc.from_string(" "),
    block(body_docs),
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
      // Check if this is an InputObject
      case dict.get(schema_types, name) {
        Ok(schema.InputObjectType(_, _, _)) ->
          call_doc(snake_case(name) <> "_to_json", [doc.from_string(var_name)])
        _ -> call_doc("json.string", [doc.from_string(var_name)])
      }
    }
  }
}

fn build_query_string(operation: parser.Operation) -> String {
  let op_type = case parser.get_operation_type(operation) {
    parser.Query -> "query"
    parser.Mutation -> "mutation"
    parser.Subscription -> "subscription"
  }

  let op_name = case parser.get_operation_name(operation) {
    Some(name) -> " " <> name
    None -> ""
  }

  let variables = parser.get_variables(operation)
  let var_defs = case variables {
    [] -> ""
    vars -> {
      let defs =
        vars
        |> list.map(fn(var) {
          "$" <> var.name <> ": " <> type_ref_to_string(var.type_ref)
        })
        |> string.join(", ")
      "(" <> defs <> ")"
    }
  }

  let selections = parser.get_selections(operation)
  let selection_set = build_selection_set(selections)

  op_type <> op_name <> var_defs <> " " <> selection_set
}

fn build_selection_set(selections: List(parser.Selection)) -> String {
  let fields =
    selections
    |> list.map(fn(selection) {
      case selection {
        parser.FieldSelection(name, _alias, args, nested) -> {
          let args_str = format_arguments(args)
          case nested {
            [] -> name <> args_str
            subs -> name <> args_str <> " " <> build_selection_set(subs)
          }
        }
      }
    })
    |> string.join(" ")

  "{ " <> fields <> " }"
}

fn type_ref_to_string(type_ref: parser.TypeRef) -> String {
  case type_ref {
    parser.NamedTypeRef(name) -> name
    parser.ListTypeRef(inner) -> "[" <> type_ref_to_string(inner) <> "]"
    parser.NonNullTypeRef(inner) -> type_ref_to_string(inner) <> "!"
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

fn format_value(value: parser.Value) -> String {
  case value {
    parser.IntValue(i) -> int.to_string(i)
    parser.FloatValue(f) -> float.to_string(f)
    parser.StringValue(s) -> "\"" <> s <> "\""
    parser.BooleanValue(True) -> "true"
    parser.BooleanValue(False) -> "false"
    parser.NullValue -> "null"
    parser.VariableValue(name) -> "$" <> name
    parser.ListValue(values) -> {
      let formatted_values =
        values
        |> list.map(format_value)
        |> string.join(", ")
      "[" <> formatted_values <> "]"
    }
    parser.ObjectValue(fields) -> {
      let formatted_fields =
        fields
        |> list.map(fn(field) {
          let #(name, value) = field
          name <> ": " <> format_value(value)
        })
        |> string.join(", ")
      "{ " <> formatted_fields <> " }"
    }
  }
}

fn format_arguments(arguments: List(parser.Argument)) -> String {
  case arguments {
    [] -> ""
    args -> {
      let formatted_args =
        args
        |> list.map(fn(arg) {
          let parser.Argument(name, value) = arg
          name <> ": " <> format_value(value)
        })
        |> string.join(", ")
      "(" <> formatted_args <> ")"
    }
  }
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

