import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import gleam/option.{type Option}
import squall

pub type Character {
  Character(
    id: Option(String),
    name: Option(String),
    status: Option(String),
    species: Option(String),
  )
}

pub fn character_decoder() -> decode.Decoder(Character) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  use status <- decode.field("status", decode.optional(decode.string))
  use species <- decode.field("species", decode.optional(decode.string))
  decode.success(Character(id: id, name: name, status: status, species: species))
}

pub fn character_to_json(input: Character) -> json.Json {
  json.object([
    #("id", json.nullable(input.id, json.string)),
    #("name", json.nullable(input.name, json.string)),
    #("status", json.nullable(input.status, json.string)),
    #("species", json.nullable(input.species, json.string)),
  ])
}

pub type GetCharacterResponse {
  GetCharacterResponse(character: Option(Character))
}

pub fn get_character_response_decoder() -> decode.Decoder(GetCharacterResponse) {
  use character <- decode.field(
    "character",
    decode.optional(character_decoder()),
  )
  decode.success(GetCharacterResponse(character: character))
}

pub fn get_character_response_to_json(input: GetCharacterResponse) -> json.Json {
  json.object([
    #("character", json.nullable(input.character, character_to_json)),
  ])
}

pub fn get_character(
  client: squall.Client,
  id: String,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetCharacter($id: ID!) {\n  character(id: $id) {\n    id\n    name\n    status\n    species\n  }\n}\n",
    json.object([#("id", json.string(id))]),
  )
}

pub fn parse_get_character_response(
  body: String,
) -> Result(GetCharacterResponse, String) {
  squall.parse_response(body, get_character_response_decoder())
}
