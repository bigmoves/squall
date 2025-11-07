import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall
import gleam/option.{type Option}

pub type Character {
  Character(
    id: Option(String),
    name: Option(String),
    status: Option(String),
    species: Option(String),
    origin: Option(Location),
  )
}

pub fn character_decoder() -> decode.Decoder(Character) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  use status <- decode.field("status", decode.optional(decode.string))
  use species <- decode.field("species", decode.optional(decode.string))
  use origin <- decode.field("origin", decode.optional(location_decoder()))
  decode.success(Character(
    id: id,
    name: name,
    status: status,
    species: species,
    origin: origin,
  ))
}

pub type Location {
  Location(name: Option(String))
}

pub fn location_decoder() -> decode.Decoder(Location) {
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(Location(name: name))
}

pub fn character_to_json(input: Character) -> json.Json {
  json.object(
    [
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
      #("status", json.nullable(input.status, json.string)),
      #("species", json.nullable(input.species, json.string)),
      #("origin", json.nullable(input.origin, location_to_json)),
    ],
  )
}

pub fn location_to_json(input: Location) -> json.Json {
  json.object([#("name", json.nullable(input.name, json.string))])
}

pub type GetCharacterWithFragmentResponse {
  GetCharacterWithFragmentResponse(character: Option(Character))
}

pub fn get_character_with_fragment_response_decoder() -> decode.Decoder(GetCharacterWithFragmentResponse) {
  use character <- decode.field("character", decode.optional(character_decoder()))
  decode.success(GetCharacterWithFragmentResponse(character: character))
}

pub fn get_character_with_fragment_response_to_json(input: GetCharacterWithFragmentResponse) -> json.Json {
  json.object(
    [
      #("character", json.nullable(input.character, character_to_json)),
    ],
  )
}

pub fn get_character_with_fragment(client: squall.Client, id: String) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "fragment CharacterInfo on Character {\n  id\n  name\n  status\n  species\n}\n\nquery GetCharacterWithFragment($id: ID!) {\n  character(id: $id) {\n    ...CharacterInfo\n    origin {\n      name\n    }\n  }\n}\n",
    json.object([#("id", json.string(id))]),
  )
}

pub fn parse_get_character_with_fragment_response(body: String) -> Result(GetCharacterWithFragmentResponse, String) {
  squall.parse_response(body, get_character_with_fragment_response_decoder())
}
