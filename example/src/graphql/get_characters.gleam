import gleam/dynamic
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option.{type Option}
import gleam/result

pub type Characters {
  Characters(results: Option(List(Character)))
}

pub fn characters_decoder() -> dynamic.Decoder(Characters) {
  fn(data: dynamic.Dynamic) -> Result(Characters, List(dynamic.DecodeError)) {
    use results <- result.try(dynamic.field(
      "results",
      dynamic.optional(dynamic.list(character_decoder())),
    )(data))
    Ok(Characters(results: results))
  }
}

pub type Character {
  Character(
    id: Option(String),
    name: Option(String),
    status: Option(String),
    species: Option(String),
  )
}

pub fn character_decoder() -> dynamic.Decoder(Character) {
  fn(data: dynamic.Dynamic) -> Result(Character, List(dynamic.DecodeError)) {
    use id <- result.try(dynamic.field("id", dynamic.optional(dynamic.string))(
      data,
    ))
    use name <- result.try(dynamic.field(
      "name",
      dynamic.optional(dynamic.string),
    )(data))
    use status <- result.try(dynamic.field(
      "status",
      dynamic.optional(dynamic.string),
    )(data))
    use species <- result.try(dynamic.field(
      "species",
      dynamic.optional(dynamic.string),
    )(data))
    Ok(Character(id: id, name: name, status: status, species: species))
  }
}

pub type GetCharactersResponse {
  GetCharactersResponse(characters: Option(Characters))
}

pub fn get_characters_response_decoder() -> dynamic.Decoder(
  GetCharactersResponse,
) {
  fn(data: dynamic.Dynamic) -> Result(
    GetCharactersResponse,
    List(dynamic.DecodeError),
  ) {
    use characters <- result.try(dynamic.field(
      "characters",
      dynamic.optional(characters_decoder()),
    )(data))
    Ok(GetCharactersResponse(characters: characters))
  }
}

pub fn get_characters(endpoint: String) -> Result(GetCharactersResponse, String) {
  let query =
    "query GetCharacters { characters { results { id name status species } } }"
  let variables = json.object([])
  let body =
    json.object([#("query", json.string(query)), #("variables", variables)])
  use req <- result.try(
    request.to(endpoint)
    |> result.map_error(fn(_) { "Invalid endpoint URL" }),
  )
  let req =
    req
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
  get_characters_response_decoder()(data_field)
  |> result.map_error(fn(_) { "Failed to decode response data" })
}
