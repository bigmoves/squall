import gleam/fetch
import gleam/javascript/promise
import gleam/list
import gleam/option.{None, Some}
import graphql/get_characters
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import squall

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL

pub type Model {
  Model(state: State)
}

pub type State {
  Loading
  Loaded(characters: List(get_characters.Character))
  Failed(message: String)
}

fn init(_flags) -> #(Model, Effect(Msg)) {
  #(Model(Loading), fetch_characters())
}

// UPDATE

pub type Msg {
  HandleCharacters(Result(get_characters.GetCharactersResponse, String))
}

fn update(_model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    HandleCharacters(Ok(response)) -> {
      let characters = case response.characters {
        Some(chars) ->
          case chars.results {
            Some(results) -> results
            None -> []
          }
        None -> []
      }
      #(Model(Loaded(characters)), effect.none())
    }
    HandleCharacters(Error(err)) -> #(Model(Failed(err)), effect.none())
  }
}

// VIEW

fn view(model: Model) -> Element(Msg) {
  html.div([], [
    html.h1([], [html.text("Rick and Morty Characters")]),
    case model.state {
      Loading -> html.p([], [html.text("Loading characters...")])
      Failed(message) ->
        html.div([], [
          html.p([attribute.style("color", "red")], [
            html.text("Error: " <> message),
          ]),
        ])
      Loaded(characters) -> render_characters(characters)
    },
  ])
}

fn render_characters(characters: List(get_characters.Character)) -> Element(Msg) {
  html.div([], [
    html.ul(
      [],
      characters
        |> list.map(fn(character) {
          html.li([], [
            html.div([], [
              html.strong([], [
                html.text(option.unwrap(character.name, "Unknown")),
              ]),
              html.text(" - "),
              html.text(
                "Status: " <> option.unwrap(character.status, "Unknown"),
              ),
              html.text(", "),
              html.text(
                "Species: " <> option.unwrap(character.species, "Unknown"),
              ),
            ]),
          ])
        }),
    ),
  ])
}

// EFFECTS

fn fetch_characters() -> Effect(Msg) {
  effect.from(fn(dispatch) {
    let client = squall.new("https://rickandmortyapi.com/graphql", [])
    let assert Ok(req) = get_characters.get_characters(client)

    fetch.send(req)
    |> promise.map(fn(fetch_result) {
      case fetch_result {
        Ok(resp) -> {
          fetch.read_text_body(resp)
          |> promise.await(fn(text_result) {
            case text_result {
              Ok(text) -> {
                let result =
                  get_characters.parse_get_characters_response(text.body)
                dispatch(HandleCharacters(result))
                promise.resolve(Nil)
              }
              Error(_) -> {
                dispatch(
                  HandleCharacters(Error("Failed to read response body")),
                )
                promise.resolve(Nil)
              }
            }
          })
        }
        Error(_) -> {
          dispatch(HandleCharacters(Error("Failed to fetch characters")))
          promise.resolve(Nil)
        }
      }
    })

    Nil
  })
}
