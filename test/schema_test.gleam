import gleam/list
import gleeunit/should
import squall/internal/schema

// Test: Parse a simple scalar type from introspection result
pub fn parse_scalar_type_test() {
  let json_response =
    "{
      \"data\": {
        \"__schema\": {
          \"types\": [
            {
              \"name\": \"String\",
              \"kind\": \"SCALAR\",
              \"description\": \"Built-in String type\"
            }
          ]
        }
      }
    }"

  let result = schema.parse_introspection_response(json_response)

  should.be_ok(result)
  let assert Ok(schema_data) = result

  // Should have at least one type
  should.equal(schema.get_type_count(schema_data), 1)

  // Should find the String type
  let string_type = schema.find_type(schema_data, "String")
  should.be_ok(string_type)
}

// Test: Parse an object type with fields
pub fn parse_object_type_test() {
  let json_response =
    "{
      \"data\": {
        \"__schema\": {
          \"types\": [
            {
              \"name\": \"Character\",
              \"kind\": \"OBJECT\",
              \"description\": \"A character from Rick and Morty\",
              \"fields\": [
                {
                  \"name\": \"id\",
                  \"type\": {
                    \"kind\": \"NON_NULL\",
                    \"ofType\": {
                      \"kind\": \"SCALAR\",
                      \"name\": \"ID\"
                    }
                  }
                },
                {
                  \"name\": \"name\",
                  \"type\": {
                    \"kind\": \"SCALAR\",
                    \"name\": \"String\"
                  }
                }
              ]
            }
          ]
        }
      }
    }"

  let result = schema.parse_introspection_response(json_response)

  should.be_ok(result)
  let assert Ok(schema_data) = result

  // Should find the Character type
  let character_type = schema.find_type(schema_data, "Character")
  should.be_ok(character_type)

  // Character should have fields
  let assert Ok(char_type) = character_type
  let fields = schema.get_type_fields(char_type)
  should.equal(list.length(fields), 2)
}

// Test: Parse query type
pub fn parse_query_type_test() {
  let json_response =
    "{
      \"data\": {
        \"__schema\": {
          \"queryType\": {
            \"name\": \"Query\"
          },
          \"types\": [
            {
              \"name\": \"Query\",
              \"kind\": \"OBJECT\",
              \"fields\": [
                {
                  \"name\": \"character\",
                  \"type\": {
                    \"kind\": \"OBJECT\",
                    \"name\": \"Character\"
                  },
                  \"args\": [
                    {
                      \"name\": \"id\",
                      \"type\": {
                        \"kind\": \"NON_NULL\",
                        \"ofType\": {
                          \"kind\": \"SCALAR\",
                          \"name\": \"ID\"
                        }
                      }
                    }
                  ]
                }
              ]
            }
          ]
        }
      }
    }"

  let result = schema.parse_introspection_response(json_response)

  should.be_ok(result)
  let assert Ok(schema_data) = result

  // Should have query type
  let query_type_name = schema.get_query_type_name(schema_data)
  should.equal(query_type_name, Ok("Query"))
}

// Test: Handle invalid JSON
pub fn invalid_json_test() {
  let invalid_json = "{ invalid json }"

  let result = schema.parse_introspection_response(invalid_json)

  should.be_error(result)
}

// Test: Handle missing __schema field
pub fn missing_schema_field_test() {
  let json_response = "{\"data\": {}}"

  let result = schema.parse_introspection_response(json_response)

  should.be_error(result)
}
