import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import squall

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

pub fn get_character(
  client: squall.Client,
  id: String,
) -> Result(GetCharacterResponse, String) {
  let query =
    "query GetCharacter($id: ID!) { character(id: $id) { id name status species type gender } }"
  let variables = json.object([#("id", json.string(id))])
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
    use data <- decode.field("data", get_character_response_decoder())
    decode.success(data)
  }
  decode.run(json_value, data_and_response_decoder)
  |> result.map_error(fn(_) { "Failed to decode response data" })
}
