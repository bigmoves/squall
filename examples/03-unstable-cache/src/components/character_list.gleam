import generated/queries/get_characters
import gleam/json
import gleam/list
import gleam/option
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import squall_cache.{type Cache}

/// Fetch all characters from Rick and Morty API
///
/// ```graphql
/// query GetCharacters {
///   characters {
///     results {
///       id
///       name
///       status
///       species
///     }
///   }
/// }
/// ```
pub fn view(cache: Cache) -> Element(msg) {
  // Just read from cache - fetching already happened in init
  // Pass generated parser directly!
  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetCharacters",
      json.object([]),
      get_characters.parse_get_characters_response,
    )

  html.div([], [
    html.h2([], [html.text("Characters")]),
    case result {
      squall_cache.Loading -> html.p([], [html.text("Loading characters...")])

      squall_cache.Failed(message) ->
        html.p([attribute.style("color", "red")], [
          html.text("Error: " <> message),
        ])

      squall_cache.Data(data) -> render_characters(data)
    },
  ])
}

fn render_characters(data: get_characters.GetCharactersResponse) -> Element(msg) {
  case data.characters {
    option.None -> html.p([], [html.text("No characters data")])
    option.Some(characters) ->
      case characters.results {
        option.None -> html.p([], [html.text("No results")])
        option.Some(results) -> {
          html.ul(
            [],
            results
              |> list.map(fn(char) {
                html.li([attribute.style("margin", "10px 0")], [
                  html.div([], [
                    html.a(
                      [
                        attribute.href(
                          "/character/" <> option.unwrap(char.id, ""),
                        ),
                      ],
                      [
                        html.strong([], [
                          html.text(
                            char.name
                            |> option.unwrap("Unknown"),
                          ),
                        ]),
                      ],
                    ),
                    html.text(
                      " - Status: " <> option.unwrap(char.status, "Unknown"),
                    ),
                    html.text(
                      ", Species: " <> option.unwrap(char.species, "Unknown"),
                    ),
                  ]),
                ])
              }),
          )
        }
      }
  }
}
