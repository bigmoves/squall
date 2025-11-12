import generated/queries/get_locations
import gleam/json
import gleam/list
import gleam/option
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import squall_cache.{type Cache}

/// Fetch all locations from Rick and Morty API
///
/// ```graphql
/// query GetLocations {
///   locations {
///     results {
///       id
///       name
///       type
///       dimension
///     }
///   }
/// }
/// ```
pub fn view(cache: Cache) -> Element(msg) {
  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetLocations",
      json.object([]),
      get_locations.parse_get_locations_response,
    )

  html.div([], [
    html.h2([], [html.text("Locations")]),
    case result {
      squall_cache.Loading -> html.p([], [html.text("Loading locations...")])

      squall_cache.Failed(message) ->
        html.p([attribute.style("color", "red")], [
          html.text("Error: " <> message),
        ])

      squall_cache.Data(data) -> render_locations(data)
    },
  ])
}

fn render_locations(data: get_locations.GetLocationsResponse) -> Element(msg) {
  case data.locations {
    option.None -> html.p([], [html.text("No locations data")])
    option.Some(locations) ->
      case locations.results {
        option.None -> html.p([], [html.text("No results")])
        option.Some(results) -> {
          html.ul(
            [],
            results
              |> list.map(fn(loc) {
                html.li([attribute.style("margin", "10px 0")], [
                  html.div([], [
                    html.strong([], [
                      html.text(
                        loc.name
                        |> option.unwrap("Unknown"),
                      ),
                    ]),
                    html.text(
                      " - Type: " <> option.unwrap(loc.type_, "Unknown"),
                    ),
                    html.text(
                      ", Dimension: " <> option.unwrap(loc.dimension, "Unknown"),
                    ),
                  ]),
                ])
              }),
          )
        }
      }
  }
}
