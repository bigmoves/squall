import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall
import gleam/option.{type Option}

pub type Character {
  Character(
    typename: Option(String),
    id: Option(String),
    name: Option(String),
    status: Option(String),
    species: Option(String),
    type_: Option(String),
    gender: Option(String),
    image: Option(String),
  )
}

pub fn character_decoder() -> decode.Decoder(Character) {
  use typename <- decode.field("__typename", decode.optional(decode.string))
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  use status <- decode.field("status", decode.optional(decode.string))
  use species <- decode.field("species", decode.optional(decode.string))
  use type_ <- decode.field("type", decode.optional(decode.string))
  use gender <- decode.field("gender", decode.optional(decode.string))
  use image <- decode.field("image", decode.optional(decode.string))
  decode.success(Character(
    typename: typename,
    id: id,
    name: name,
    status: status,
    species: species,
    type_: type_,
    gender: gender,
    image: image,
  ))
}

pub fn character_to_json(input: Character) -> json.Json {
  json.object(
    [
      #("__typename", json.nullable(input.typename, json.string)),
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
      #("status", json.nullable(input.status, json.string)),
      #("species", json.nullable(input.species, json.string)),
      #("type", json.nullable(input.type_, json.string)),
      #("gender", json.nullable(input.gender, json.string)),
      #("image", json.nullable(input.image, json.string)),
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

pub fn get_character(client: squall.Client, id: String) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetCharacter($id: ID!) {\n  character(id: $id) {\n    __typename\n    id\n    name\n    status\n    species\n    type\n    gender\n    image\n  }\n}",
    json.object([#("id", json.string(id))]),
  )
}

pub fn parse_get_character_response(body: String) -> Result(GetCharacterResponse, String) {
  squall.parse_response(body, get_character_response_decoder())
}
