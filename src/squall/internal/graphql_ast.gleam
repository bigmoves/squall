/// Adapter module that wraps swell's GraphQL parser and provides
/// helper functions for working with the AST in Squall's code generation.
/// This provides a clean interface that the rest of Squall can use.
import gleam/list
import gleam/option.{type Option}
import gleam/result
import squall/internal/error.{type Error}
import swell/lexer
import swell/parser

// Re-export swell's types so other modules don't need to import swell directly
pub type Operation =
  parser.Operation

pub type Selection =
  parser.Selection

pub type SelectionSet =
  parser.SelectionSet

pub type Variable =
  parser.Variable

pub type Argument =
  parser.Argument

pub type ArgumentValue =
  parser.ArgumentValue

pub type Document =
  parser.Document

// Operation type enum (for easier pattern matching)
pub type OperationType {
  Query
  Mutation
  Subscription
}

/// Parse a GraphQL query string into a full Document
/// This includes all operations (queries, mutations, fragment definitions)
pub fn parse_document(source: String) -> Result(Document, Error) {
  case parser.parse(source) {
    Ok(document) -> Ok(document)
    Error(parser.LexerError(lexer_error)) -> {
      case lexer_error {
        lexer.UnexpectedCharacter(char, pos) ->
          Error(error.InvalidGraphQLSyntax(
            "lexer",
            pos,
            "Unexpected character: " <> char,
          ))
        lexer.UnterminatedString(pos) ->
          Error(error.InvalidGraphQLSyntax("lexer", pos, "Unterminated string"))
        lexer.InvalidNumber(num, pos) ->
          Error(error.InvalidGraphQLSyntax(
            "lexer",
            pos,
            "Invalid number: " <> num,
          ))
      }
    }
    Error(parser.UnexpectedToken(_token, msg)) ->
      Error(error.InvalidGraphQLSyntax("parser", 0, msg))
    Error(parser.UnexpectedEndOfInput(msg)) ->
      Error(error.InvalidGraphQLSyntax("parser", 0, msg))
  }
}

/// Parse a GraphQL query string into an Operation
/// Returns the first operation from the document
pub fn parse(source: String) -> Result(Operation, Error) {
  case parser.parse(source) {
    Ok(parser.Document(operations)) -> {
      case operations {
        [first, ..] -> Ok(first)
        [] ->
          Error(error.InvalidGraphQLSyntax(
            "parse",
            0,
            "No operations found in document",
          ))
      }
    }
    Error(parser.LexerError(lexer_error)) -> {
      case lexer_error {
        lexer.UnexpectedCharacter(char, pos) ->
          Error(error.InvalidGraphQLSyntax(
            "lexer",
            pos,
            "Unexpected character: " <> char,
          ))
        lexer.UnterminatedString(pos) ->
          Error(error.InvalidGraphQLSyntax("lexer", pos, "Unterminated string"))
        lexer.InvalidNumber(num, pos) ->
          Error(error.InvalidGraphQLSyntax(
            "lexer",
            pos,
            "Invalid number: " <> num,
          ))
      }
    }
    Error(parser.UnexpectedToken(_token, msg)) ->
      Error(error.InvalidGraphQLSyntax("parser", 0, msg))
    Error(parser.UnexpectedEndOfInput(msg)) ->
      Error(error.InvalidGraphQLSyntax("parser", 0, msg))
  }
}

/// Get the operation type (Query, Mutation, or Subscription)
pub fn get_operation_type(operation: Operation) -> OperationType {
  case operation {
    parser.Query(_) | parser.NamedQuery(_, _, _) -> Query
    parser.Mutation(_) | parser.NamedMutation(_, _, _) -> Mutation
    parser.Subscription(_) | parser.NamedSubscription(_, _, _) -> Subscription
    parser.FragmentDefinition(_, _, _) -> Query
    // Fragments are treated as queries for error handling
  }
}

/// Get the operation name (if it has one)
pub fn get_operation_name(operation: Operation) -> Option(String) {
  case operation {
    parser.Query(_) | parser.Mutation(_) | parser.Subscription(_) -> option.None
    parser.NamedQuery(name, _, _)
    | parser.NamedMutation(name, _, _)
    | parser.NamedSubscription(name, _, _)
    | parser.FragmentDefinition(name, _, _) -> option.Some(name)
  }
}

/// Get the selections from an operation
pub fn get_selections(operation: Operation) -> List(Selection) {
  case operation {
    parser.Query(parser.SelectionSet(selections))
    | parser.Mutation(parser.SelectionSet(selections))
    | parser.Subscription(parser.SelectionSet(selections)) -> selections
    parser.NamedQuery(_, _, parser.SelectionSet(selections))
    | parser.NamedMutation(_, _, parser.SelectionSet(selections))
    | parser.NamedSubscription(_, _, parser.SelectionSet(selections))
    | parser.FragmentDefinition(_, _, parser.SelectionSet(selections)) ->
      selections
  }
}

/// Get the variables from an operation
pub fn get_variables(operation: Operation) -> List(Variable) {
  case operation {
    parser.Query(_) | parser.Mutation(_) | parser.Subscription(_) -> []
    parser.NamedQuery(_, variables, _)
    | parser.NamedMutation(_, variables, _)
    | parser.NamedSubscription(_, variables, _) -> variables
    parser.FragmentDefinition(_, _, _) -> []
  }
}

/// Get the variable name from a Variable
pub fn get_variable_name(variable: Variable) -> String {
  let parser.Variable(name, _) = variable
  name
}

/// Get the variable type as a string (swell stores types as strings)
pub fn get_variable_type_string(variable: Variable) -> String {
  let parser.Variable(_, type_str) = variable
  type_str
}

/// Get all fragment definitions from a document
pub fn get_fragment_definitions(document: Document) -> List(Operation) {
  let parser.Document(operations) = document
  operations
  |> list.filter(fn(op) {
    case op {
      parser.FragmentDefinition(_, _, _) -> True
      _ -> False
    }
  })
}

/// Get the first non-fragment operation from a document
/// This is typically the query or mutation to execute
pub fn get_main_operation(document: Document) -> Result(Operation, Error) {
  let parser.Document(operations) = document
  operations
  |> list.find(fn(op) {
    case op {
      parser.FragmentDefinition(_, _, _) -> False
      _ -> True
    }
  })
  |> result.replace_error(error.InvalidGraphQLSyntax(
    "parse",
    0,
    "No executable operation found in document",
  ))
}

/// Find a fragment definition by name
pub fn find_fragment(
  fragments: List(Operation),
  name: String,
) -> Option(Operation) {
  fragments
  |> list.find(fn(fragment) {
    case fragment {
      parser.FragmentDefinition(fragment_name, _, _) -> fragment_name == name
      _ -> False
    }
  })
  |> option.from_result
}

/// Expand all fragment spreads in a list of selections
/// This recursively expands fragments and handles nested fragments
pub fn expand_fragments(
  selections: List(Selection),
  fragments: List(Operation),
) -> Result(List(Selection), Error) {
  selections
  |> list.try_map(fn(selection) {
    case selection {
      parser.Field(name, alias, arguments, nested_selections) -> {
        // Recursively expand fragments in nested selections
        use expanded_nested <- result.try(expand_fragments(
          nested_selections,
          fragments,
        ))
        Ok([parser.Field(name, alias, arguments, expanded_nested)])
      }
      parser.FragmentSpread(fragment_name) -> {
        // Find the fragment and expand its selections
        case find_fragment(fragments, fragment_name) {
          option.Some(fragment) -> {
            let fragment_selections = get_selections(fragment)
            // Recursively expand in case fragment contains other fragments
            expand_fragments(fragment_selections, fragments)
          }
          option.None ->
            Error(error.InvalidGraphQLSyntax(
              "fragment",
              0,
              "Fragment '" <> fragment_name <> "' is not defined",
            ))
        }
      }
      parser.InlineFragment(_type_condition, inline_selections) -> {
        // Expand fragments in the inline fragment's selections
        expand_fragments(inline_selections, fragments)
      }
    }
  })
  |> result.map(list.flatten)
}
