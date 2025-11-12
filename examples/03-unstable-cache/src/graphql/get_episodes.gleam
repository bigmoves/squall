import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall
import gleam/option.{type Option}

pub type Episodes {
  Episodes(results: Option(List(Episode)))
}

pub fn episodes_decoder() -> decode.Decoder(Episodes) {
  use results <- decode.field("results", decode.optional(decode.list(episode_decoder())))
  decode.success(Episodes(results: results))
}

pub type Episode {
  Episode(
    id: Option(String),
    name: Option(String),
    air_date: Option(String),
    episode: Option(String),
  )
}

pub fn episode_decoder() -> decode.Decoder(Episode) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  use air_date <- decode.field("air_date", decode.optional(decode.string))
  use episode <- decode.field("episode", decode.optional(decode.string))
  decode.success(Episode(
    id: id,
    name: name,
    air_date: air_date,
    episode: episode,
  ))
}

pub fn episodes_to_json(input: Episodes) -> json.Json {
  json.object(
    [
      #("results", json.nullable(
        input.results,
        fn(list) { json.array(from: list, of: episode_to_json) },
      )),
    ],
  )
}

pub fn episode_to_json(input: Episode) -> json.Json {
  json.object(
    [
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
      #("air_date", json.nullable(input.air_date, json.string)),
      #("episode", json.nullable(input.episode, json.string)),
    ],
  )
}

pub type GetEpisodesResponse {
  GetEpisodesResponse(episodes: Option(Episodes))
}

pub fn get_episodes_response_decoder() -> decode.Decoder(GetEpisodesResponse) {
  use episodes <- decode.field("episodes", decode.optional(episodes_decoder()))
  decode.success(GetEpisodesResponse(episodes: episodes))
}

pub fn get_episodes_response_to_json(input: GetEpisodesResponse) -> json.Json {
  json.object([#("episodes", json.nullable(input.episodes, episodes_to_json))])
}

pub fn get_episodes(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetEpisodes {\n  episodes {\n    results {\n      id\n      name\n      air_date\n      episode\n    }\n  }\n}\n",
    json.object([]),
  )
}

pub fn parse_get_episodes_response(body: String) -> Result(GetEpisodesResponse, String) {
  squall.parse_response(body, get_episodes_response_decoder())
}
