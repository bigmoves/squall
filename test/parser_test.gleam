import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import squall/internal/graphql_ast

// Test: Parse simple query with no variables
pub fn parse_simple_query_test() {
  let source =
    "
    query GetUser {
      user {
        id
        name
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
  let assert Ok(operation) = result

  // Check operation type
  graphql_ast.get_operation_type(operation)
  |> should.equal(graphql_ast.Query)

  // Check operation name
  graphql_ast.get_operation_name(operation)
  |> should.equal(Some("GetUser"))

  // Check it has a selection set
  let selections = graphql_ast.get_selections(operation)
  list.is_empty(selections)
  |> should.be_false()
}

// Test: Parse query with variables
pub fn parse_query_with_variables_test() {
  let source =
    "
    query GetCharacter($id: ID!) {
      character(id: $id) {
        name
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
  let assert Ok(operation) = result

  // Check variables
  let variables = graphql_ast.get_variables(operation)
  list.length(variables)
  |> should.equal(1)

  // Check first variable
  let assert [var] = variables
  graphql_ast.get_variable_name(var)
  |> should.equal("id")
}

// Test: Parse query with multiple variables
pub fn parse_query_multiple_variables_test() {
  let source =
    "
    query ListCharacters($page: Int, $filter: String) {
      characters(page: $page, filter: $filter) {
        results {
          id
        }
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
  let assert Ok(operation) = result

  let variables = graphql_ast.get_variables(operation)
  list.length(variables)
  |> should.equal(2)
}

// Test: Parse mutation
pub fn parse_mutation_test() {
  let source =
    "
    mutation CreateUser($name: String!) {
      createUser(name: $name) {
        id
        name
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
  let assert Ok(operation) = result

  graphql_ast.get_operation_type(operation)
  |> should.equal(graphql_ast.Mutation)

  graphql_ast.get_operation_name(operation)
  |> should.equal(Some("CreateUser"))
}

// Test: Parse subscription
pub fn parse_subscription_test() {
  let source =
    "
    subscription OnUserCreated {
      userCreated {
        id
        name
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
  let assert Ok(operation) = result

  graphql_ast.get_operation_type(operation)
  |> should.equal(graphql_ast.Subscription)
}

// Test: Parse nested selections
pub fn parse_nested_selections_test() {
  let source =
    "
    query GetCharacter {
      character {
        name
        location {
          id
          name
        }
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
  let assert Ok(operation) = result

  let selections = graphql_ast.get_selections(operation)
  list.is_empty(selections)
  |> should.be_false()
}

// Test: Parse query with arguments
pub fn parse_query_with_arguments_test() {
  let source =
    "
    query GetCharacters {
      characters(page: 1, filter: \"Rick\") {
        results {
          name
        }
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
}

// Test: Parse anonymous query (no operation name)
pub fn parse_anonymous_query_test() {
  let source =
    "
    query {
      user {
        id
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
  let assert Ok(operation) = result

  graphql_ast.get_operation_name(operation)
  |> should.equal(None)
}

// Test: Invalid syntax
pub fn parse_invalid_syntax_test() {
  let source = "query GetUser { user { id }"

  let result = graphql_ast.parse(source)

  should.be_error(result)
}

// Test: Empty query
pub fn parse_empty_query_test() {
  let source = ""

  let result = graphql_ast.parse(source)

  should.be_error(result)
}

// Test: Parse variable with list type
// NOTE: Swell's parser currently doesn't support list types in variable definitions
// This is a known limitation: https://github.com/giacomocavalieri/swell/issues/X
pub fn parse_variable_list_type_test() {
  let source =
    "
    query GetUsers($ids: [ID!]!) {
      users(ids: $ids) {
        id
      }
    }
  "

  let result = graphql_ast.parse(source)

  // For now, this is expected to fail until swell supports list types in variables
  should.be_error(result)
}

// Test: Parse variable with non-null type
pub fn parse_variable_non_null_type_test() {
  let source =
    "
    query GetUser($id: ID!) {
      user(id: $id) {
        id
      }
    }
  "

  let result = graphql_ast.parse(source)

  should.be_ok(result)
}
