import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import squall/internal/error.{type Error}
import squall/internal/graphql_ast
import squall/internal/schema
import swell/parser

/// Automatically inject __typename into selection sets for Object/Interface/Union types
/// Only injects if __typename is not already present
pub fn inject_typename(
  query_string: String,
  schema_data: schema.Schema,
) -> Result(String, Error) {
  // Parse the query string into an AST
  use document <- result.try(graphql_ast.parse_document(query_string))

  let parser.Document(operations) = document

  // Process each operation
  let modified_operations =
    list.map(operations, fn(operation) {
      process_operation(operation, schema_data)
    })

  // Reconstruct document
  let modified_document = parser.Document(modified_operations)

  // Serialize back to string
  Ok(serialize_document(modified_document))
}

/// Process a single operation to inject __typename
fn process_operation(
  operation: parser.Operation,
  schema_data: schema.Schema,
) -> parser.Operation {
  case operation {
    parser.Query(parser.SelectionSet(selections)) -> {
      let root_type = get_root_type(schema_data.query_type, schema_data)
      let modified_selections =
        inject_into_selections(selections, root_type, schema_data, True)
      parser.Query(parser.SelectionSet(modified_selections))
    }
    parser.NamedQuery(name, variables, parser.SelectionSet(selections)) -> {
      let root_type = get_root_type(schema_data.query_type, schema_data)
      let modified_selections =
        inject_into_selections(selections, root_type, schema_data, True)
      parser.NamedQuery(
        name,
        variables,
        parser.SelectionSet(modified_selections),
      )
    }
    parser.Mutation(parser.SelectionSet(selections)) -> {
      let root_type = get_root_type(schema_data.mutation_type, schema_data)
      let modified_selections =
        inject_into_selections(selections, root_type, schema_data, True)
      parser.Mutation(parser.SelectionSet(modified_selections))
    }
    parser.NamedMutation(name, variables, parser.SelectionSet(selections)) -> {
      let root_type = get_root_type(schema_data.mutation_type, schema_data)
      let modified_selections =
        inject_into_selections(selections, root_type, schema_data, True)
      parser.NamedMutation(
        name,
        variables,
        parser.SelectionSet(modified_selections),
      )
    }
    parser.Subscription(parser.SelectionSet(selections)) -> {
      let root_type = get_root_type(schema_data.subscription_type, schema_data)
      let modified_selections =
        inject_into_selections(selections, root_type, schema_data, True)
      parser.Subscription(parser.SelectionSet(modified_selections))
    }
    parser.NamedSubscription(name, variables, parser.SelectionSet(selections)) -> {
      let root_type = get_root_type(schema_data.subscription_type, schema_data)
      let modified_selections =
        inject_into_selections(selections, root_type, schema_data, True)
      parser.NamedSubscription(
        name,
        variables,
        parser.SelectionSet(modified_selections),
      )
    }
    parser.FragmentDefinition(
      name,
      type_condition,
      parser.SelectionSet(selections),
    ) -> {
      let fragment_type = case dict.get(schema_data.types, type_condition) {
        Ok(t) -> t
        Error(_) -> schema.ScalarType(type_condition, None)
      }
      let modified_selections =
        inject_into_selections(selections, fragment_type, schema_data, False)
      parser.FragmentDefinition(
        name,
        type_condition,
        parser.SelectionSet(modified_selections),
      )
    }
  }
}

/// Get root type from optional type name
fn get_root_type(
  type_name: option.Option(String),
  schema_data: schema.Schema,
) -> schema.Type {
  case type_name {
    Some(name) ->
      case dict.get(schema_data.types, name) {
        Ok(t) -> t
        Error(_) -> schema.ScalarType(name, None)
      }
    None -> schema.ScalarType("Query", None)
  }
}

/// Recursively inject __typename into selections
/// is_root: True if this is the root Query/Mutation/Subscription level (don't inject there)
fn inject_into_selections(
  selections: List(parser.Selection),
  parent_type: schema.Type,
  schema_data: schema.Schema,
  is_root: Bool,
) -> List(parser.Selection) {
  // Check if __typename is already present
  let has_typename =
    list.any(selections, fn(selection) {
      case selection {
        parser.Field("__typename", _, _, _) -> True
        _ -> False
      }
    })

  // Determine if we should inject __typename for this parent type
  // Don't inject at root level (Query/Mutation/Subscription)
  let should_inject = case is_root {
    True -> False
    False ->
      case parent_type {
        schema.ObjectType(_, _, _) -> True
        schema.InterfaceType(_, _, _) -> True
        schema.UnionType(_, _, _) -> True
        _ -> False
      }
  }

  // Process each selection and inject into nested selections
  let processed_selections =
    list.map(selections, fn(selection) {
      case selection {
        parser.Field(field_name, alias, args, nested_selections) -> {
          case nested_selections {
            [] -> selection
            _ -> {
              // Get the field's type from the schema
              let fields = schema.get_type_fields(parent_type)
              let field_type = case
                list.find(fields, fn(f) { f.name == field_name })
              {
                Ok(field) -> {
                  let type_name = get_base_type_name(field.type_ref)
                  case dict.get(schema_data.types, type_name) {
                    Ok(t) -> t
                    Error(_) -> schema.ScalarType(type_name, None)
                  }
                }
                Error(_) -> schema.ScalarType("Unknown", None)
              }

              // Recursively inject into nested selections (never root)
              let modified_nested =
                inject_into_selections(
                  nested_selections,
                  field_type,
                  schema_data,
                  False,
                )
              parser.Field(field_name, alias, args, modified_nested)
            }
          }
        }
        parser.InlineFragment(type_condition, nested_selections) -> {
          // Get the type for the inline fragment
          let fragment_type = case type_condition {
            Some(type_name) ->
              case dict.get(schema_data.types, type_name) {
                Ok(t) -> t
                Error(_) -> schema.ScalarType(type_name, None)
              }
            None -> parent_type
          }
          let modified_nested =
            inject_into_selections(
              nested_selections,
              fragment_type,
              schema_data,
              False,
            )
          parser.InlineFragment(type_condition, modified_nested)
        }
        parser.FragmentSpread(_) -> selection
      }
    })

  // Inject __typename if needed
  case should_inject && !has_typename {
    True -> {
      // Prepend __typename to the selections
      [parser.Field("__typename", None, [], []), ..processed_selections]
    }
    False -> processed_selections
  }
}

/// Extract the base type name from a TypeRef (unwrap NonNull and List)
fn get_base_type_name(type_ref: schema.TypeRef) -> String {
  case type_ref {
    schema.NamedType(name, _) -> name
    schema.NonNullType(inner) -> get_base_type_name(inner)
    schema.ListType(inner) -> get_base_type_name(inner)
  }
}

/// Serialize a GraphQL document back to a string
fn serialize_document(document: parser.Document) -> String {
  let parser.Document(operations) = document

  operations
  |> list.map(serialize_operation)
  |> list.intersperse("\n\n")
  |> list.fold("", fn(acc, s) { acc <> s })
}

/// Serialize an operation to a string
fn serialize_operation(operation: parser.Operation) -> String {
  case operation {
    parser.Query(selection_set) -> {
      "query " <> serialize_selection_set(selection_set, 0)
    }
    parser.NamedQuery(name, variables, selection_set) -> {
      "query "
      <> name
      <> serialize_variables(variables)
      <> " "
      <> serialize_selection_set(selection_set, 0)
    }
    parser.Mutation(selection_set) -> {
      "mutation " <> serialize_selection_set(selection_set, 0)
    }
    parser.NamedMutation(name, variables, selection_set) -> {
      "mutation "
      <> name
      <> serialize_variables(variables)
      <> " "
      <> serialize_selection_set(selection_set, 0)
    }
    parser.Subscription(selection_set) -> {
      "subscription " <> serialize_selection_set(selection_set, 0)
    }
    parser.NamedSubscription(name, variables, selection_set) -> {
      "subscription "
      <> name
      <> serialize_variables(variables)
      <> " "
      <> serialize_selection_set(selection_set, 0)
    }
    parser.FragmentDefinition(name, type_condition, selection_set) -> {
      "fragment "
      <> name
      <> " on "
      <> type_condition
      <> " "
      <> serialize_selection_set(selection_set, 0)
    }
  }
}

/// Serialize variables
fn serialize_variables(variables: List(parser.Variable)) -> String {
  case variables {
    [] -> ""
    vars -> {
      let vars_str =
        vars
        |> list.map(fn(var) {
          let parser.Variable(name, type_str) = var
          "$" <> name <> ": " <> type_str
        })
        |> list.intersperse(", ")
        |> list.fold("", fn(acc, s) { acc <> s })
      "(" <> vars_str <> ")"
    }
  }
}

/// Serialize a selection set
fn serialize_selection_set(
  selection_set: parser.SelectionSet,
  indent: Int,
) -> String {
  let parser.SelectionSet(selections) = selection_set
  let indent_str =
    list.repeat("  ", indent) |> list.fold("", fn(acc, s) { acc <> s })
  let next_indent = indent + 1
  let next_indent_str =
    list.repeat("  ", next_indent) |> list.fold("", fn(acc, s) { acc <> s })

  let selections_str =
    selections
    |> list.map(fn(sel) {
      next_indent_str <> serialize_selection(sel, next_indent)
    })
    |> list.intersperse("\n")
    |> list.fold("", fn(acc, s) { acc <> s })

  "{\n" <> selections_str <> "\n" <> indent_str <> "}"
}

/// Serialize a single selection
fn serialize_selection(selection: parser.Selection, indent: Int) -> String {
  case selection {
    parser.Field(name, alias, args, nested) -> {
      let alias_str = case alias {
        Some(a) -> a <> ": "
        None -> ""
      }
      let args_str = serialize_arguments(args)
      case nested {
        [] -> alias_str <> name <> args_str
        _ ->
          alias_str
          <> name
          <> args_str
          <> " "
          <> serialize_selection_set(parser.SelectionSet(nested), indent)
      }
    }
    parser.InlineFragment(type_condition, nested) -> {
      let type_str = case type_condition {
        Some(t) -> " on " <> t
        None -> ""
      }
      "..."
      <> type_str
      <> " "
      <> serialize_selection_set(parser.SelectionSet(nested), indent)
    }
    parser.FragmentSpread(name) -> "..." <> name
  }
}

/// Serialize arguments
fn serialize_arguments(args: List(parser.Argument)) -> String {
  case args {
    [] -> ""
    arguments -> {
      let args_str =
        arguments
        |> list.map(fn(arg) {
          let parser.Argument(name, value) = arg
          name <> ": " <> serialize_argument_value(value)
        })
        |> list.intersperse(", ")
        |> list.fold("", fn(acc, s) { acc <> s })
      "(" <> args_str <> ")"
    }
  }
}

/// Serialize an argument value
fn serialize_argument_value(value: parser.ArgumentValue) -> String {
  case value {
    parser.IntValue(i) -> i
    parser.FloatValue(f) -> f
    parser.StringValue(s) -> "\"" <> s <> "\""
    parser.BooleanValue(True) -> "true"
    parser.BooleanValue(False) -> "false"
    parser.NullValue -> "null"
    parser.EnumValue(e) -> e
    parser.ListValue(values) -> {
      let values_str =
        values
        |> list.map(serialize_argument_value)
        |> list.intersperse(", ")
        |> list.fold("", fn(acc, s) { acc <> s })
      "[" <> values_str <> "]"
    }
    parser.ObjectValue(fields) -> {
      let fields_str =
        fields
        |> list.map(fn(field) {
          let #(field_name, field_value) = field
          field_name <> ": " <> serialize_argument_value(field_value)
        })
        |> list.intersperse(", ")
        |> list.fold("", fn(acc, s) { acc <> s })
      "{" <> fields_str <> "}"
    }
    parser.VariableValue(name) -> "$" <> name
  }
}
