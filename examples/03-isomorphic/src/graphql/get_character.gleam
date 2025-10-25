import gleam/dynamic/decode
import gleam/json
import squall
import gleam/option.{type Option}

pub type Character {
  Character(
    id: Option(String),
    name: Option(String),
    status: Option(String),
    species: Option(String),
    type_: Option(String),
    gender: Option(String),
  )
}

pub fn character_decoder() -> decode.Decoder(Character) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  use status <- decode.field("status", decode.optional(decode.string))
  use species <- decode.field("species", decode.optional(decode.string))
  use type_ <- decode.field("type", decode.optional(decode.string))
  use gender <- decode.field("gender", decode.optional(decode.string))
  decode.success(Character(
    id: id,
    name: name,
    status: status,
    species: species,
    type_: type_,
    gender: gender,
  ))
}

pub fn character_to_json(input: Character) -> json.Json {
  json.object(
    [
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
      #("status", json.nullable(input.status, json.string)),
      #("species", json.nullable(input.species, json.string)),
      #("type", json.nullable(input.type_, json.string)),
      #("gender", json.nullable(input.gender, json.string)),
    ],
  )
}

pub type GetCharacterResponse {
  GetCharacterResponse(character: Option(Character))
}

pub fn get_character_response_decoder() -> decode.Decoder(GetCharacterResponse) {
  use character <- decode.field("character", decode.optional(character_decoder()))
  decode.success(GetCharacterResponse(character: character))
}

pub fn get_character_response_to_json(input: GetCharacterResponse) -> json.Json {
  json.object(
    [
      #("character", json.nullable(input.character, character_to_json)),
    ],
  )
}

pub fn get_character(client: squall.Client, id: String) {
  squall.execute_query(
    client,
    "query GetCharacter($id: ID!) { character(id: $id) { id name status species type gender } }",
    json.object([#("id", json.string(id))]),
    get_character_response_decoder(),
  )
}
