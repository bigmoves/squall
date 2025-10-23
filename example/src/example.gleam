import gleam/io
import graphql/get_characters

pub fn main() {
  let result =
    get_characters.get_characters("https://rickandmortyapi.com/graphql")

  case result {
    Ok(response) -> {
      // Handle response with list of characters
      io.println("Successfully fetched characters!")
      io.debug(response)
      Nil
    }
    Error(err) -> {
      io.println("Error: " <> err)
    }
  }
}
