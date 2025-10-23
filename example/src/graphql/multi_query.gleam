import gleam/dynamic
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option.{type Option}
import gleam/result

pub type Characters {
  Characters(info: Option(Info), results: Option(List(Character)))
}

pub fn characters_decoder() -> dynamic.Decoder(Characters) {
  fn(data: dynamic.Dynamic) -> Result(Characters, List(dynamic.DecodeError)) {
    use info <- result.try(dynamic.field(
      "info",
      dynamic.optional(info_decoder()),
    )(data))
    use results <- result.try(dynamic.field(
      "results",
      dynamic.optional(dynamic.list(character_decoder())),
    )(data))
    Ok(Characters(info: info, results: results))
  }
}

pub type Info {
  Info(count: Option(Int))
}

pub fn info_decoder() -> dynamic.Decoder(Info) {
  fn(data: dynamic.Dynamic) -> Result(Info, List(dynamic.DecodeError)) {
    use count <- result.try(dynamic.field(
      "count",
      dynamic.optional(dynamic.int),
    )(data))
    Ok(Info(count: count))
  }
}

pub type Character {
  Character(name: Option(String))
}

pub fn character_decoder() -> dynamic.Decoder(Character) {
  fn(data: dynamic.Dynamic) -> Result(Character, List(dynamic.DecodeError)) {
    use name <- result.try(dynamic.field(
      "name",
      dynamic.optional(dynamic.string),
    )(data))
    Ok(Character(name: name))
  }
}

pub type Location {
  Location(id: Option(String))
}

pub fn location_decoder() -> dynamic.Decoder(Location) {
  fn(data: dynamic.Dynamic) -> Result(Location, List(dynamic.DecodeError)) {
    use id <- result.try(dynamic.field("id", dynamic.optional(dynamic.string))(data))
    Ok(Location(id: id))
  }
}

pub type Episode {
  Episode(id: Option(String))
}

pub fn episode_decoder() -> dynamic.Decoder(Episode) {
  fn(data: dynamic.Dynamic) -> Result(Episode, List(dynamic.DecodeError)) {
    use id <- result.try(dynamic.field("id", dynamic.optional(dynamic.string))(data))
    Ok(Episode(id: id))
  }
}

pub type MultiQueryResponse {
  MultiQueryResponse(
    characters: Option(Characters),
    location: Option(Location),
    episodes_by_ids: Option(List(Episode)),
  )
}

pub fn multi_query_response_decoder() -> dynamic.Decoder(MultiQueryResponse) {
  fn(data: dynamic.Dynamic) -> Result(MultiQueryResponse, List(dynamic.DecodeError)) {
    use characters <- result.try(dynamic.field(
      "characters",
      dynamic.optional(characters_decoder()),
    )(data))
    use location <- result.try(dynamic.field(
      "location",
      dynamic.optional(location_decoder()),
    )(data))
    use episodes_by_ids <- result.try(dynamic.field(
      "episodesByIds",
      dynamic.optional(dynamic.list(episode_decoder())),
    )(data))
    Ok(
      MultiQueryResponse(
        characters: characters,
        location: location,
        episodes_by_ids: episodes_by_ids,
      ),
    )
  }
}

pub fn multi_query(endpoint: String) -> Result(MultiQueryResponse, String) {
  let query =
  "query MultiQuery { characters(page: 2, filter: { name: \"rick\" }) { info { count } results { name } } location(id: 1) { id } episodesByIds(ids: [1, 2]) { id } }"
  let variables = json.object([])
  let body =
  json.object([#("query", json.string(query)), #("variables", variables)])
  use req <- result.try(
    request.to(endpoint)
    |> result.map_error(fn(_) { "Invalid endpoint URL" }),
  )
  let req = req
  |> request.set_method(http.Post)
  |> request.set_body(json.to_string(body))
  |> request.set_header("content-type", "application/json")
  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { "HTTP request failed" }),
  )
  use json_value <- result.try(
    json.decode(from: resp.body, using: dynamic.dynamic)
    |> result.map_error(fn(_) { "Failed to decode JSON response" }),
  )
  use data_field <- result.try(
    dynamic.field("data", dynamic.dynamic)(json_value)
    |> result.map_error(fn(_) { "No data field in response" }),
  )
  multi_query_response_decoder()(data_field)
  |> result.map_error(fn(_) { "Failed to decode response data" })
}
