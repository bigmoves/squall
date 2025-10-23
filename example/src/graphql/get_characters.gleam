import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import squall

pub type Characters {
  Characters(results: Option(List(Character)))
}

pub fn characters_decoder() -> decode.Decoder(Characters) {
  use results <- decode.field("results", decode.optional(decode.list(character_decoder())))
  decode.success(Characters(results: results))
}

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

pub type GetCharactersResponse {
  GetCharactersResponse(characters: Option(Characters))
}

pub fn get_characters_response_decoder() -> decode.Decoder(GetCharactersResponse) {
  use characters <- decode.field("characters", decode.optional(characters_decoder()))
  decode.success(GetCharactersResponse(characters: characters))
}

pub fn get_characters(client: squall.Client) -> Result(GetCharactersResponse, String) {
  let query =
    "query GetCharacters { characters { results { id name status species } } }"
  let variables =
    json.object([])
  let body =
    json.object([#("query", json.string(query)), #("variables", variables)])
  use req <- result.try(
    request.to(client.endpoint)
    |> result.map_error(fn(_) { "Invalid endpoint URL" }),
  )
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_body(json.to_string(body))
    |> request.set_header("content-type", "application/json")
  let req =
    list.fold(client.headers, req, fn(r, header) {
      request.set_header(r, header.0, header.1)
    })
  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { "HTTP request failed" }),
  )
  use json_value <- result.try(
    json.parse(from: resp.body, using: decode.dynamic)
    |> result.map_error(fn(_) { "Failed to decode JSON response" }),
  )
  let data_and_response_decoder = {
    use data <- decode.field("data", get_characters_response_decoder())
    decode.success(data)
  }
  decode.run(json_value, data_and_response_decoder)
  |> result.map_error(fn(_) { "Failed to decode response data" })
}
