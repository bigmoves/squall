import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall
import gleam/option.{type Option}

pub type Characters {
  Characters(typename: Option(String), results: Option(List(Character)))
}

pub fn characters_decoder() -> decode.Decoder(Characters) {
  use typename <- decode.field("__typename", decode.optional(decode.string))
  use results <- decode.field("results", decode.optional(decode.list(character_decoder())))
  decode.success(Characters(typename: typename, results: results))
}

pub type Character {
  Character(
    typename: Option(String),
    id: Option(String),
    name: Option(String),
    status: Option(String),
    species: Option(String),
  )
}

pub fn character_decoder() -> decode.Decoder(Character) {
  use typename <- decode.field("__typename", decode.optional(decode.string))
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  use status <- decode.field("status", decode.optional(decode.string))
  use species <- decode.field("species", decode.optional(decode.string))
  decode.success(Character(
    typename: typename,
    id: id,
    name: name,
    status: status,
    species: species,
  ))
}

pub fn characters_to_json(input: Characters) -> json.Json {
  json.object(
    [
      #("__typename", json.nullable(input.typename, json.string)),
      #("results", json.nullable(
        input.results,
        fn(list) { json.array(from: list, of: character_to_json) },
      )),
    ],
  )
}

pub fn character_to_json(input: Character) -> json.Json {
  json.object(
    [
      #("__typename", json.nullable(input.typename, json.string)),
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
      #("status", json.nullable(input.status, json.string)),
      #("species", json.nullable(input.species, json.string)),
    ],
  )
}

pub type GetCharactersResponse {
  GetCharactersResponse(characters: Option(Characters))
}

pub fn get_characters_response_decoder() -> decode.Decoder(GetCharactersResponse) {
  use characters <- decode.field("characters", decode.optional(characters_decoder()))
  decode.success(GetCharactersResponse(characters: characters))
}

pub fn get_characters_response_to_json(input: GetCharactersResponse) -> json.Json {
  json.object(
    [
      #("characters", json.nullable(input.characters, characters_to_json)),
    ],
  )
}

pub fn get_characters(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetCharacters {\n  characters {\n    __typename\n    results {\n      __typename\n      id\n      name\n      status\n      species\n    }\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_characters_response(body: String) -> Result(GetCharactersResponse, String) {
  squall.parse_response(body, get_characters_response_decoder())
}
