import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import squall/internal/error.{type Error}

// AST types
pub type OperationType {
  Query
  Mutation
  Subscription
}

pub type Operation {
  Operation(
    operation_type: OperationType,
    name: Option(String),
    variables: List(Variable),
    selections: List(Selection),
  )
}

pub type Variable {
  Variable(name: String, type_ref: TypeRef)
}

pub type TypeRef {
  NamedTypeRef(name: String)
  ListTypeRef(inner: TypeRef)
  NonNullTypeRef(inner: TypeRef)
}

pub type Selection {
  FieldSelection(
    name: String,
    alias: Option(String),
    arguments: List(Argument),
    selections: List(Selection),
  )
}

pub type Argument {
  Argument(name: String, value: Value)
}

pub type Value {
  IntValue(value: Int)
  FloatValue(value: Float)
  StringValue(value: String)
  BooleanValue(value: Bool)
  NullValue
  VariableValue(name: String)
  ListValue(values: List(Value))
  ObjectValue(fields: List(#(String, Value)))
}

// Token types
type Token {
  LeftBrace
  RightBrace
  LeftParen
  RightParen
  LeftBracket
  RightBracket
  Colon
  Comma
  Exclamation
  Dollar
  Equals
  At
  Name(String)
  StringLit(String)
  IntLit(Int)
  FloatLit(Float)
  EOF
}

type TokenPosition {
  TokenPosition(token: Token, line: Int, column: Int)
}

// Parser state
type ParserState {
  ParserState(tokens: List(TokenPosition), position: Int)
}

// Public API

pub fn parse(source: String) -> Result(Operation, Error) {
  use tokens <- result.try(tokenize(source))
  parse_operation(ParserState(tokens, 0))
}

pub fn get_operation_type(op: Operation) -> OperationType {
  op.operation_type
}

pub fn get_operation_name(op: Operation) -> Option(String) {
  op.name
}

pub fn get_variables(op: Operation) -> List(Variable) {
  op.variables
}

pub fn get_selections(op: Operation) -> List(Selection) {
  op.selections
}

pub fn get_variable_name(var: Variable) -> String {
  var.name
}

// Lexer (Tokenizer)

fn tokenize(source: String) -> Result(List(TokenPosition), Error) {
  tokenize_helper(source, 1, 1, [])
  |> result.map(list.reverse)
}

fn tokenize_helper(
  source: String,
  line: Int,
  col: Int,
  acc: List(TokenPosition),
) -> Result(List(TokenPosition), Error) {
  case string.pop_grapheme(source) {
    Error(_) -> Ok([TokenPosition(EOF, line, col), ..acc])
    Ok(#(char, rest)) -> {
      case char {
        // Whitespace
        " " | "\t" -> tokenize_helper(rest, line, col + 1, acc)
        "\n" -> tokenize_helper(rest, line + 1, 1, acc)
        "\r" -> tokenize_helper(rest, line, col, acc)

        // Single character tokens
        "{" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(LeftBrace, line, col),
            ..acc
          ])
        }
        "}" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(RightBrace, line, col),
            ..acc
          ])
        }
        "(" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(LeftParen, line, col),
            ..acc
          ])
        }
        ")" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(RightParen, line, col),
            ..acc
          ])
        }
        "[" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(LeftBracket, line, col),
            ..acc
          ])
        }
        "]" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(RightBracket, line, col),
            ..acc
          ])
        }
        ":" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(Colon, line, col),
            ..acc
          ])
        }
        "," -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(Comma, line, col),
            ..acc
          ])
        }
        "!" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(Exclamation, line, col),
            ..acc
          ])
        }
        "$" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(Dollar, line, col),
            ..acc
          ])
        }
        "=" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(Equals, line, col),
            ..acc
          ])
        }
        "@" -> {
          tokenize_helper(rest, line, col + 1, [
            TokenPosition(At, line, col),
            ..acc
          ])
        }

        // String literal
        "\"" -> {
          use #(str_val, remaining, new_col) <- result.try(read_string(
            rest,
            "",
            col + 1,
          ))
          tokenize_helper(remaining, line, new_col, [
            TokenPosition(StringLit(str_val), line, col),
            ..acc
          ])
        }

        // Numbers or names
        _ -> {
          case is_alpha(char) || char == "_" {
            True -> {
              let #(name, remaining, new_col) = read_name(char <> rest, col)
              tokenize_helper(remaining, line, new_col, [
                TokenPosition(Name(name), line, col),
                ..acc
              ])
            }
            False ->
              case is_digit(char) || char == "-" {
                True -> {
                  use #(num_token, remaining, new_col) <- result.try(
                    read_number(char <> rest, col, line),
                  )
                  tokenize_helper(remaining, line, new_col, [
                    TokenPosition(num_token, line, col),
                    ..acc
                  ])
                }
                False -> {
                  Error(error.InvalidGraphQLSyntax(
                    "tokenize",
                    line,
                    "Unexpected character: " <> char,
                  ))
                }
              }
          }
        }
      }
    }
  }
}

fn read_string(
  source: String,
  acc: String,
  col: Int,
) -> Result(#(String, String, Int), Error) {
  case string.pop_grapheme(source) {
    Error(_) -> Error(error.InvalidGraphQLSyntax("string", 0, "Unterminated string"))
    Ok(#("\"", rest)) -> Ok(#(acc, rest, col + 1))
    Ok(#("\\", rest)) -> {
      case string.pop_grapheme(rest) {
        Ok(#(escaped, rest2)) -> {
          read_string(rest2, acc <> escaped, col + 2)
        }
        Error(_) -> Error(error.InvalidGraphQLSyntax("string", 0, "Unterminated string"))
      }
    }
    Ok(#(char, rest)) -> read_string(rest, acc <> char, col + 1)
  }
}

fn read_name(source: String, col: Int) -> #(String, String, Int) {
  read_name_helper(source, "", col)
}

fn read_name_helper(source: String, acc: String, col: Int) -> #(String, String, Int) {
  case string.pop_grapheme(source) {
    Error(_) -> #(acc, "", col)
    Ok(#(char, rest)) -> {
      case is_alpha(char) || is_digit(char) || char == "_" {
        True -> read_name_helper(rest, acc <> char, col + 1)
        False -> #(acc, char <> rest, col)
      }
    }
  }
}

fn read_number(
  source: String,
  col: Int,
  line: Int,
) -> Result(#(Token, String, Int), Error) {
  let #(num_str, rest, new_col) = read_number_helper(source, "", col)

  case string.contains(num_str, ".") || string.contains(num_str, "e") || string.contains(num_str, "E") {
    True -> {
      // Try to parse as float
      case try_parse_float(num_str) {
        Ok(f) -> Ok(#(FloatLit(f), rest, new_col))
        Error(_) ->
          Error(error.InvalidGraphQLSyntax(
            "number",
            line,
            "Invalid float: " <> num_str,
          ))
      }
    }
    False -> {
      // Try to parse as int
      case try_parse_int(num_str) {
        Ok(i) -> Ok(#(IntLit(i), rest, new_col))
        Error(_) ->
          Error(error.InvalidGraphQLSyntax(
            "number",
            line,
            "Invalid int: " <> num_str,
          ))
      }
    }
  }
}

fn try_parse_int(s: String) -> Result(Int, Nil) {
  case s {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    _ -> parse_int(s)
  }
}

fn try_parse_float(s: String) -> Result(Float, Nil) {
  // For floats without 'e', ensure it has decimal point
  case string.contains(s, ".") {
    False -> Error(Nil)
    True -> parse_float(s)
  }
}

fn read_number_helper(source: String, acc: String, col: Int) -> #(String, String, Int) {
  case string.pop_grapheme(source) {
    Error(_) -> #(acc, "", col)
    Ok(#(char, rest)) -> {
      case is_digit(char) || char == "." || char == "-" || char == "e" || char == "E" {
        True -> read_number_helper(rest, acc <> char, col + 1)
        False -> #(acc, char <> rest, col)
      }
    }
  }
}

fn is_alpha(char: String) -> Bool {
  case char {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ->
      True
    _ -> False
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

// Simple int/float parsing (Gleam stdlib might have better versions)
@external(erlang, "erlang", "binary_to_integer")
fn parse_int(s: String) -> Result(Int, Nil)

@external(erlang, "erlang", "binary_to_float")
fn parse_float(s: String) -> Result(Float, Nil)

// Parser

fn parse_operation(state: ParserState) -> Result(Operation, Error) {
  // Skip whitespace/comments
  let state = skip_insignificant(state)

  // Check for operation type keyword
  use #(op_type, state) <- result.try(parse_operation_type(state))

  // Try to parse operation name
  let #(op_name, state) = parse_optional_name(state)

  // Parse variables
  use #(variables, state) <- result.try(parse_variable_definitions(state))

  // Parse selection set
  use #(selections, _state) <- result.try(parse_selection_set(state))

  Ok(Operation(op_type, op_name, variables, selections))
}

fn parse_operation_type(
  state: ParserState,
) -> Result(#(OperationType, ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    Name("query") -> Ok(#(Query, advance(state)))
    Name("mutation") -> Ok(#(Mutation, advance(state)))
    Name("subscription") -> Ok(#(Subscription, advance(state)))
    _ ->
      Error(error.InvalidGraphQLSyntax(
        "operation",
        token_pos.line,
        "Expected 'query', 'mutation', or 'subscription'",
      ))
  }
}

fn parse_optional_name(state: ParserState) -> #(Option(String), ParserState) {
  case peek_token(state) {
    Ok(TokenPosition(Name(name), _, _)) -> #(Some(name), advance(state))
    _ -> #(None, state)
  }
}

fn parse_variable_definitions(
  state: ParserState,
) -> Result(#(List(Variable), ParserState), Error) {
  case peek_token(state) {
    Ok(TokenPosition(LeftParen, _, _)) -> {
      let state = advance(state)
      parse_variable_list(state, [])
    }
    _ -> Ok(#([], state))
  }
}

fn parse_variable_list(
  state: ParserState,
  acc: List(Variable),
) -> Result(#(List(Variable), ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    RightParen -> Ok(#(list.reverse(acc), advance(state)))
    Comma -> parse_variable_list(advance(state), acc)
    Dollar -> {
      use #(var, state) <- result.try(parse_variable(state))
      parse_variable_list(state, [var, ..acc])
    }
    _ ->
      Error(error.InvalidGraphQLSyntax(
        "variables",
        token_pos.line,
        "Expected '$' or ')'",
      ))
  }
}

fn parse_variable(state: ParserState) -> Result(#(Variable, ParserState), Error) {
  // Expect $
  use state <- result.try(expect_token(state, Dollar, "variable"))

  // Parse variable name
  use #(name, state) <- result.try(parse_name(state))

  // Expect :
  use state <- result.try(expect_token(state, Colon, "variable type"))

  // Parse type reference
  use #(type_ref, state) <- result.try(parse_type_ref(state))

  Ok(#(Variable(name, type_ref), state))
}

fn parse_type_ref(state: ParserState) -> Result(#(TypeRef, ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    LeftBracket -> {
      let state = advance(state)
      use #(inner, state) <- result.try(parse_type_ref(state))
      use state <- result.try(expect_token(state, RightBracket, "list type"))

      // Check for non-null
      case peek_token(state) {
        Ok(TokenPosition(Exclamation, _, _)) ->
          Ok(#(NonNullTypeRef(ListTypeRef(inner)), advance(state)))
        _ -> Ok(#(ListTypeRef(inner), state))
      }
    }

    Name(name) -> {
      let state = advance(state)

      // Check for non-null
      case peek_token(state) {
        Ok(TokenPosition(Exclamation, _, _)) ->
          Ok(#(NonNullTypeRef(NamedTypeRef(name)), advance(state)))
        _ -> Ok(#(NamedTypeRef(name), state))
      }
    }

    _ ->
      Error(error.InvalidGraphQLSyntax(
        "type",
        token_pos.line,
        "Expected type name or '['",
      ))
  }
}

fn parse_selection_set(
  state: ParserState,
) -> Result(#(List(Selection), ParserState), Error) {
  use state <- result.try(expect_token(state, LeftBrace, "selection set"))
  parse_selection_list(state, [])
}

fn parse_selection_list(
  state: ParserState,
  acc: List(Selection),
) -> Result(#(List(Selection), ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    RightBrace -> Ok(#(list.reverse(acc), advance(state)))
    Name(_) -> {
      use #(selection, state) <- result.try(parse_field(state))
      parse_selection_list(state, [selection, ..acc])
    }
    _ ->
      Error(error.InvalidGraphQLSyntax(
        "selection",
        token_pos.line,
        "Expected field name or '}'",
      ))
  }
}

fn parse_field(state: ParserState) -> Result(#(Selection, ParserState), Error) {
  use #(name, state) <- result.try(parse_name(state))

  // Check for alias (field: realField)
  let #(alias, field_name, state) = case peek_token(state) {
    Ok(TokenPosition(Colon, _, _)) -> {
      let state = advance(state)
      case parse_name(state) {
        Ok(#(real_name, state)) -> #(Some(name), real_name, state)
        Error(_) -> #(None, name, state)
      }
    }
    _ -> #(None, name, state)
  }

  // Parse arguments if present
  use #(arguments, state) <- result.try(parse_arguments(state))

  // Parse nested selections if present
  let #(selections, state) = case peek_token(state) {
    Ok(TokenPosition(LeftBrace, _, _)) -> {
      case parse_selection_set(state) {
        Ok(#(sels, state)) -> #(sels, state)
        Error(_) -> #([], state)
      }
    }
    _ -> #([], state)
  }

  Ok(#(FieldSelection(field_name, alias, arguments, selections), state))
}

fn parse_arguments(
  state: ParserState,
) -> Result(#(List(Argument), ParserState), Error) {
  case peek_token(state) {
    Ok(TokenPosition(LeftParen, _, _)) -> {
      let state = advance(state)
      parse_argument_list(state, [])
    }
    _ -> Ok(#([], state))
  }
}

fn parse_argument_list(
  state: ParserState,
  acc: List(Argument),
) -> Result(#(List(Argument), ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    RightParen -> Ok(#(list.reverse(acc), advance(state)))
    Comma -> parse_argument_list(advance(state), acc)
    Name(_) -> {
      use #(arg, state) <- result.try(parse_argument(state))
      parse_argument_list(state, [arg, ..acc])
    }
    _ ->
      Error(error.InvalidGraphQLSyntax(
        "argument",
        token_pos.line,
        "Expected argument name or ')'",
      ))
  }
}

fn parse_argument(state: ParserState) -> Result(#(Argument, ParserState), Error) {
  use #(name, state) <- result.try(parse_name(state))
  use state <- result.try(expect_token(state, Colon, "argument value"))
  use #(value, state) <- result.try(parse_value(state))

  Ok(#(Argument(name, value), state))
}

fn parse_value(state: ParserState) -> Result(#(Value, ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    IntLit(i) -> Ok(#(IntValue(i), advance(state)))
    FloatLit(f) -> Ok(#(FloatValue(f), advance(state)))
    StringLit(s) -> Ok(#(StringValue(s), advance(state)))
    Name("true") -> Ok(#(BooleanValue(True), advance(state)))
    Name("false") -> Ok(#(BooleanValue(False), advance(state)))
    Name("null") -> Ok(#(NullValue, advance(state)))
    Dollar -> {
      let state = advance(state)
      use #(name, state) <- result.try(parse_name(state))
      Ok(#(VariableValue(name), state))
    }
    LeftBracket -> {
      let state = advance(state)
      parse_list_value(state, [])
    }
    LeftBrace -> {
      let state = advance(state)
      parse_object_value(state, [])
    }
    _ ->
      Error(error.InvalidGraphQLSyntax(
        "value",
        token_pos.line,
        "Expected a value",
      ))
  }
}

fn parse_list_value(
  state: ParserState,
  acc: List(Value),
) -> Result(#(Value, ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    RightBracket -> Ok(#(ListValue(list.reverse(acc)), advance(state)))
    Comma -> parse_list_value(advance(state), acc)
    _ -> {
      use #(value, state) <- result.try(parse_value(state))
      parse_list_value(state, [value, ..acc])
    }
  }
}

fn parse_object_value(
  state: ParserState,
  acc: List(#(String, Value)),
) -> Result(#(Value, ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    RightBrace -> Ok(#(ObjectValue(list.reverse(acc)), advance(state)))
    Comma -> parse_object_value(advance(state), acc)
    Name(_) -> {
      use #(name, state) <- result.try(parse_name(state))
      use state <- result.try(expect_token(state, Colon, "object field value"))
      use #(value, state) <- result.try(parse_value(state))
      parse_object_value(state, [#(name, value), ..acc])
    }
    _ ->
      Error(error.InvalidGraphQLSyntax(
        "object",
        token_pos.line,
        "Expected field name or '}'",
      ))
  }
}

fn parse_name(state: ParserState) -> Result(#(String, ParserState), Error) {
  use token_pos <- result.try(peek_token(state))

  case token_pos.token {
    Name(name) -> Ok(#(name, advance(state)))
    _ ->
      Error(error.InvalidGraphQLSyntax(
        "name",
        token_pos.line,
        "Expected a name",
      ))
  }
}

// Helper functions

fn peek_token(state: ParserState) -> Result(TokenPosition, Error) {
  case list.drop(state.tokens, state.position) {
    [token, ..] -> Ok(token)
    [] -> Error(error.InvalidGraphQLSyntax("parser", 0, "Unexpected end of input"))
  }
}

fn advance(state: ParserState) -> ParserState {
  ParserState(..state, position: state.position + 1)
}

fn expect_token(
  state: ParserState,
  expected: Token,
  context: String,
) -> Result(ParserState, Error) {
  use token_pos <- result.try(peek_token(state))

  case tokens_equal(token_pos.token, expected) {
    True -> Ok(advance(state))
    False ->
      Error(error.InvalidGraphQLSyntax(
        context,
        token_pos.line,
        "Expected " <> token_to_string(expected),
      ))
  }
}

fn tokens_equal(a: Token, b: Token) -> Bool {
  case a, b {
    LeftBrace, LeftBrace -> True
    RightBrace, RightBrace -> True
    LeftParen, LeftParen -> True
    RightParen, RightParen -> True
    LeftBracket, LeftBracket -> True
    RightBracket, RightBracket -> True
    Colon, Colon -> True
    Comma, Comma -> True
    Exclamation, Exclamation -> True
    Dollar, Dollar -> True
    Equals, Equals -> True
    At, At -> True
    EOF, EOF -> True
    _, _ -> False
  }
}

fn token_to_string(token: Token) -> String {
  case token {
    LeftBrace -> "'{'"
    RightBrace -> "'}'"
    LeftParen -> "'('"
    RightParen -> "')'"
    LeftBracket -> "'['"
    RightBracket -> "']'"
    Colon -> "':'"
    Comma -> "','"
    Exclamation -> "'!'"
    Dollar -> "'$'"
    Equals -> "'='"
    At -> "'@'"
    Name(n) -> "name '" <> n <> "'"
    StringLit(s) -> "string \"" <> s <> "\""
    IntLit(_) -> "integer"
    FloatLit(_) -> "float"
    EOF -> "end of input"
  }
}

fn skip_insignificant(state: ParserState) -> ParserState {
  state
}
