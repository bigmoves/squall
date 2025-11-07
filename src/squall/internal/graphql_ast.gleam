/// Adapter module that wraps swell's GraphQL parser and provides
/// helper functions for working with the AST in Squall's code generation.
/// This provides a clean interface that the rest of Squall can use.
import gleam/option.{type Option}
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
