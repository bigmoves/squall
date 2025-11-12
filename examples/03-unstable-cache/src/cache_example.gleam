import components/character_detail
import components/character_list
import components/episode_list
import components/location_list
import generated/queries
import generated/queries/get_character
import generated/queries/get_characters
import generated/queries/get_episodes
import generated/queries/get_locations
import gleam/json.{type Json}
import gleam/string
import gleam/uri
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import modem
import squall/registry
import squall_cache

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL

pub type Route {
  Home
  Characters
  CharacterDetail(id: String)
  Episodes
  Locations
}

pub type Model {
  Model(cache: squall_cache.Cache, registry: registry.Registry, route: Route)
}

fn init(_flags) -> #(Model, Effect(Msg)) {
  // Create cache - optionally with headers for authentication
  let cache = squall_cache.new("https://rickandmortyapi.com/graphql")
  // To add headers (e.g., for authentication):
  // let cache = squall_cache.new_with_headers(fn() {
  //   [#("Authorization", "Bearer " <> get_auth_token())]
  // })

  let reg = queries.init_registry()

  // Parse the initial route from the current URL
  let initial_route = case modem.initial_uri() {
    Ok(uri) -> parse_route(uri)
    Error(_) -> Home
  }

  // Determine what data needs to be fetched based on initial route
  let #(initial_cache, data_effects) = case initial_route {
    Characters -> {
      let #(new_cache, _) =
        squall_cache.lookup(
          cache,
          "GetCharacters",
          json.object([]),
          get_characters.parse_get_characters_response,
        )
      let #(final_cache, fx) =
        squall_cache.process_pending(new_cache, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    CharacterDetail(id) -> {
      let variables = json.object([#("id", json.string(id))])
      let #(new_cache, _) =
        squall_cache.lookup(
          cache,
          "GetCharacter",
          variables,
          get_character.parse_get_character_response,
        )
      let #(final_cache, fx) =
        squall_cache.process_pending(new_cache, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    Episodes -> {
      let #(new_cache, _) =
        squall_cache.lookup(
          cache,
          "GetEpisodes",
          json.object([]),
          get_episodes.parse_get_episodes_response,
        )
      let #(final_cache, fx) =
        squall_cache.process_pending(new_cache, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    Locations -> {
      let #(new_cache, _) =
        squall_cache.lookup(
          cache,
          "GetLocations",
          json.object([]),
          get_locations.parse_get_locations_response,
        )
      let #(final_cache, fx) =
        squall_cache.process_pending(new_cache, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    Home -> #(cache, [])
  }

  // Combine modem effect with data fetching effects
  let modem_effect = modem.init(on_url_change)
  let combined_effects = effect.batch([modem_effect, ..data_effects])

  #(
    Model(cache: initial_cache, registry: reg, route: initial_route),
    combined_effects,
  )
}

// UPDATE

pub type Msg {
  HandleQueryResponse(String, Json, Result(String, String))
  OnRouteChange(Route)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    HandleQueryResponse(query_name, variables, Ok(response_body)) -> {
      // Store response in cache as raw string with the correct variables
      let cache_with_data =
        squall_cache.store_query(
          model.cache,
          query_name,
          variables,
          response_body,
          0,
        )

      // Process any new pending fetches
      let #(final_cache, effects) =
        squall_cache.process_pending(
          cache_with_data,
          model.registry,
          HandleQueryResponse,
          fn() { 0 },
        )

      #(Model(..model, cache: final_cache), effect.batch(effects))
    }

    HandleQueryResponse(_query_name, _variables, Error(_err)) -> {
      #(model, effect.none())
    }

    OnRouteChange(route) -> {
      // When route changes, trigger lookups for that route's data
      let effects = case route {
        Characters -> {
          let #(new_cache, _result) =
            squall_cache.lookup(
              model.cache,
              "GetCharacters",
              json.object([]),
              get_characters.parse_get_characters_response,
            )
          let #(final_cache, fx) =
            squall_cache.process_pending(
              new_cache,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )
          [#(final_cache, fx)]
        }
        CharacterDetail(id) -> {
          let variables = json.object([#("id", json.string(id))])
          let #(new_cache, _result) =
            squall_cache.lookup(
              model.cache,
              "GetCharacter",
              variables,
              get_character.parse_get_character_response,
            )
          let #(final_cache, fx) =
            squall_cache.process_pending(
              new_cache,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )
          [#(final_cache, fx)]
        }
        Episodes -> {
          let #(new_cache, _result) =
            squall_cache.lookup(
              model.cache,
              "GetEpisodes",
              json.object([]),
              get_episodes.parse_get_episodes_response,
            )
          let #(final_cache, fx) =
            squall_cache.process_pending(
              new_cache,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )
          [#(final_cache, fx)]
        }
        Locations -> {
          let #(new_cache, _result) =
            squall_cache.lookup(
              model.cache,
              "GetLocations",
              json.object([]),
              get_locations.parse_get_locations_response,
            )
          let #(final_cache, fx) =
            squall_cache.process_pending(
              new_cache,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )
          [#(final_cache, fx)]
        }
        Home -> []
      }

      case effects {
        [#(final_cache, fx)] -> #(
          Model(..model, cache: final_cache, route: route),
          effect.batch(fx),
        )
        _ -> #(Model(..model, route: route), effect.none())
      }
    }
  }
}

// VIEW

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.style("padding", "20px")], [
    html.h1([], [html.text("Rick and Morty API - Cached")]),
    html.p([], [
      html.text(
        "Navigate between pages - watch how cached data loads instantly!",
      ),
    ]),
    // Navigation
    html.nav([attribute.style("margin", "20px 0")], [
      html.a([attribute.href("/"), attribute.style("margin-right", "10px")], [
        html.text("Home"),
      ]),
      html.a(
        [attribute.href("/characters"), attribute.style("margin-right", "10px")],
        [html.text("Characters")],
      ),
      html.a(
        [attribute.href("/episodes"), attribute.style("margin-right", "10px")],
        [html.text("Episodes")],
      ),
      html.a([attribute.href("/locations")], [html.text("Locations")]),
    ]),
    html.hr([]),
    // Route content
    case model.route {
      Home ->
        html.div([], [
          html.h2([], [html.text("Welcome!")]),
          html.p([], [
            html.text(
              "Click the links above to explore. Notice how data loads instantly on revisit!",
            ),
          ]),
        ])
      Characters -> character_list.view(model.cache)
      CharacterDetail(id) -> character_detail.view(model.cache, id)
      Episodes -> episode_list.view(model.cache)
      Locations -> location_list.view(model.cache)
    },
  ])
}

// ROUTING

fn on_url_change(uri: uri.Uri) -> Msg {
  OnRouteChange(parse_route(uri))
}

fn parse_route(uri: uri.Uri) -> Route {
  case uri.path {
    "/" -> Home
    "/characters" -> Characters
    "/episodes" -> Episodes
    "/locations" -> Locations
    _ -> {
      // Try to parse /character/:id
      case string.split(uri.path, "/") {
        ["", "character", id] -> CharacterDetail(id)
        _ -> Home
      }
    }
  }
}
