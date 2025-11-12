import generated/queries/get_character
import gleam/json
import gleam/option
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import squall_cache.{type Cache}

/// Fetch a single character by ID from Rick and Morty API
///
/// ```graphql
/// query GetCharacter($id: ID!) {
///   character(id: $id) {
///     id
///     name
///     status
///     species
///     type
///     gender
///     image
///   }
/// }
/// ```
pub fn view(cache: Cache, id: String) -> Element(msg) {
  let variables = json.object([#("id", json.string(id))])

  let #(_cache, result) =
    squall_cache.lookup(
      cache,
      "GetCharacter",
      variables,
      get_character.parse_get_character_response,
    )

  html.div([], [
    html.h2([], [html.text("Character Detail")]),
    case result {
      squall_cache.Loading -> html.p([], [html.text("Loading character...")])

      squall_cache.Failed(message) ->
        html.p([attribute.style("color", "red")], [
          html.text("Error: " <> message),
        ])

      squall_cache.Data(data) -> render_character(data)
    },
  ])
}

fn render_character(data: get_character.GetCharacterResponse) -> Element(msg) {
  case data.character {
    option.None -> html.p([], [html.text("Character not found")])
    option.Some(char) -> {
      html.div([attribute.style("max-width", "600px")], [
        html.div(
          [
            attribute.style("display", "flex"),
            attribute.style("gap", "20px"),
          ],
          [
            case char.image {
              option.Some(img) ->
                html.img([
                  attribute.src(img),
                  attribute.style("width", "300px"),
                  attribute.style("border-radius", "8px"),
                ])
              option.None -> html.div([], [])
            },
            html.div([], [
              html.h3([], [
                html.text(char.name |> option.unwrap("Unknown")),
              ]),
              html.p([], [
                html.strong([], [html.text("Status: ")]),
                html.text(char.status |> option.unwrap("Unknown")),
              ]),
              html.p([], [
                html.strong([], [html.text("Species: ")]),
                html.text(char.species |> option.unwrap("Unknown")),
              ]),
              html.p([], [
                html.strong([], [html.text("Gender: ")]),
                html.text(char.gender |> option.unwrap("Unknown")),
              ]),
              case char.type_ {
                option.Some("") | option.None -> html.div([], [])
                option.Some(type_) ->
                  html.p([], [
                    html.strong([], [html.text("Type: ")]),
                    html.text(type_),
                  ])
              },
            ]),
          ],
        ),
      ])
    }
  }
}
