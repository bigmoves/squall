import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall
import gleam/option.{type Option}

pub type Locations {
  Locations(typename: Option(String), results: Option(List(Location)))
}

pub fn locations_decoder() -> decode.Decoder(Locations) {
  use typename <- decode.field("__typename", decode.optional(decode.string))
  use results <- decode.field("results", decode.optional(decode.list(location_decoder())))
  decode.success(Locations(typename: typename, results: results))
}

pub type Location {
  Location(
    typename: Option(String),
    id: Option(String),
    name: Option(String),
    type_: Option(String),
    dimension: Option(String),
  )
}

pub fn location_decoder() -> decode.Decoder(Location) {
  use typename <- decode.field("__typename", decode.optional(decode.string))
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.optional(decode.string))
  use type_ <- decode.field("type", decode.optional(decode.string))
  use dimension <- decode.field("dimension", decode.optional(decode.string))
  decode.success(Location(
    typename: typename,
    id: id,
    name: name,
    type_: type_,
    dimension: dimension,
  ))
}

pub fn locations_to_json(input: Locations) -> json.Json {
  json.object(
    [
      #("__typename", json.nullable(input.typename, json.string)),
      #("results", json.nullable(
        input.results,
        fn(list) { json.array(from: list, of: location_to_json) },
      )),
    ],
  )
}

pub fn location_to_json(input: Location) -> json.Json {
  json.object(
    [
      #("__typename", json.nullable(input.typename, json.string)),
      #("id", json.nullable(input.id, json.string)),
      #("name", json.nullable(input.name, json.string)),
      #("type", json.nullable(input.type_, json.string)),
      #("dimension", json.nullable(input.dimension, json.string)),
    ],
  )
}

pub type GetLocationsResponse {
  GetLocationsResponse(locations: Option(Locations))
}

pub fn get_locations_response_decoder() -> decode.Decoder(GetLocationsResponse) {
  use locations <- decode.field("locations", decode.optional(locations_decoder()))
  decode.success(GetLocationsResponse(locations: locations))
}

pub fn get_locations_response_to_json(input: GetLocationsResponse) -> json.Json {
  json.object(
    [
      #("locations", json.nullable(input.locations, locations_to_json)),
    ],
  )
}

pub fn get_locations(client: squall.Client) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "query GetLocations {\n  locations {\n    __typename\n    results {\n      __typename\n      id\n      name\n      type\n      dimension\n    }\n  }\n}",
    json.object([]),
  )
}

pub fn parse_get_locations_response(body: String) -> Result(GetLocationsResponse, String) {
  squall.parse_response(body, get_locations_response_decoder())
}
