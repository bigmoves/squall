import gleam/dynamic/decode
import gleam/json
import squall
import gleam/option.{type Option}

pub type Characters {
  Characters(info: Option(Info), results: Option(List(Character)))
}

pub fn characters_decoder() -> decode.Decoder(Characters) {
  use info <- decode.field("info", decode.optional(info_decoder()))
  use results <- decode.field("results", decode.optional(decode.list(character_decoder())))
  decode.success(Characters(info: info, results: results))
}

pub type Info {
  Info(count: Option(Int))
}

pub fn info_decoder() -> decode.Decoder(Info) {
  use count <- decode.field("count", decode.optional(decode.int))
  decode.success(Info(count: count))
}

pub type Character {
  Character(name: Option(String))
}

pub fn character_decoder() -> decode.Decoder(Character) {
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(Character(name: name))
}

pub type Location {
  Location(id: Option(String), name: Option(String))
}

pub fn location_decoder() -> decode.Decoder(Location) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(Location(id: id, name: name))
}

pub type Episode {
  Episode(id: Option(String), name: Option(String))
}

pub fn episode_decoder() -> decode.Decoder(Episode) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(Episode(id: id, name: name))
}

pub fn characters_to_json(input: Characters) -> json.Json {
  json.object(
    [
      #("info", json.nullable(input.info, info_to_json)),
      #("results", json.nullable(
        input.results,
        fn(list) { json.array(from: list, of: character_to_json) },
      )),
    ],
  )
}

pub fn info_to_json(input: Info) -> json.Json {
  json.object([#("count", json.nullable(input.count, json.int))])
}

pub fn character_to_json(input: Character) -> json.Json {
  json.object([#("name", json.nullable(input.name, json.string))])
}

pub fn location_to_json(input: Location) -> json.Json {
  json.object(
    [
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
    ],
  )
}

pub fn episode_to_json(input: Episode) -> json.Json {
  json.object(
    [
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
    ],
  )
}

pub type MultiQueryWithVarsResponse {
  MultiQueryWithVarsResponse(
    characters: Option(Characters),
    location: Option(Location),
    episodes_by_ids: Option(List(Episode)),
  )
}

pub fn multi_query_with_vars_response_decoder() -> decode.Decoder(MultiQueryWithVarsResponse) {
  use characters <- decode.field("characters", decode.optional(characters_decoder()))
  use location <- decode.field("location", decode.optional(location_decoder()))
  use episodes_by_ids <- decode.field("episodesByIds", decode.optional(decode.list(episode_decoder())))
  decode.success(MultiQueryWithVarsResponse(
    characters: characters,
    location: location,
    episodes_by_ids: episodes_by_ids,
  ))
}

pub fn multi_query_with_vars_response_to_json(input: MultiQueryWithVarsResponse) -> json.Json {
  json.object(
    [
      #("characters", json.nullable(input.characters, characters_to_json)),
      #("location", json.nullable(input.location, location_to_json)),
      #("episodesByIds", json.nullable(
        input.episodes_by_ids,
        fn(list) { json.array(from: list, of: episode_to_json) },
      )),
    ],
  )
}

pub fn multi_query_with_vars(client: squall.Client, page: Int, name: String, location_id: String, episode_ids: List(Int)) {
  squall.execute_query(
    client,
    "query MultiQueryWithVars($page: Int, $name: String, $locationId: ID!, $episodeIds: [Int!]!) { characters(page: $page, filter: { name: $name }) { info { count } results { name } } location(id: $locationId) { id name } episodesByIds(ids: $episodeIds) { id name } }",
    json.object(
      [
        #("page", json.int(page)),
        #("name", json.string(name)),
        #("locationId", json.string(location_id)),
        #("episodeIds", json.array(from: episode_ids, of: json.int)),
      ],
    ),
    multi_query_with_vars_response_decoder(),
  )
}
