import generated/queries/get_episodes
import gleam/json
import gleam/list
import gleam/option
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import squall_cache.{type Cache}

/// Fetch all episodes from Rick and Morty API
///
/// ```graphql
/// query GetEpisodes {
///   episodes {
///     results {
///       id
///       name
///       air_date
///       episode
///     }
///   }
/// }
/// ```
pub fn view(cache: Cache) -> Element(msg) {
  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetEpisodes",
      json.object([]),
      get_episodes.parse_get_episodes_response,
    )

  html.div([], [
    html.h2([], [html.text("Episodes")]),
    case result {
      squall_cache.Loading -> html.p([], [html.text("Loading episodes...")])

      squall_cache.Failed(message) ->
        html.p([attribute.style("color", "red")], [
          html.text("Error: " <> message),
        ])

      squall_cache.Data(data) -> render_episodes(data)
    },
  ])
}

fn render_episodes(data: get_episodes.GetEpisodesResponse) -> Element(msg) {
  case data.episodes {
    option.None -> html.p([], [html.text("No episodes data")])
    option.Some(episodes) ->
      case episodes.results {
        option.None -> html.p([], [html.text("No results")])
        option.Some(results) -> {
          html.ul(
            [],
            results
              |> list.map(fn(ep) {
                html.li([attribute.style("margin", "10px 0")], [
                  html.div([], [
                    html.strong([], [
                      html.text(
                        ep.episode
                        |> option.unwrap(""),
                      ),
                      html.text(" - "),
                      html.text(
                        ep.name
                        |> option.unwrap("Unknown"),
                      ),
                    ]),
                    html.text(
                      " (Aired: "
                      <> option.unwrap(ep.air_date, "Unknown")
                      <> ")",
                    ),
                  ]),
                ])
              }),
          )
        }
      }
  }
}
