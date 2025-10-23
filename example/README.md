# Example: Rick and Morty API

This example demonstrates how to use Squall with the Rick and Morty GraphQL API.

## API Endpoint

```
https://rickandmortyapi.com/graphql
```

## Queries

### get_character.gql

Fetches a single character by ID.

**Variables:**
- `id` (ID!): The character's ID

**Example:**
```gleam
import example/graphql/get_character

pub fn main() {
  let result = get_character.get_character(
    endpoint: "https://rickandmortyapi.com/graphql",
    id: "1"
  )

  case result {
    Ok(response) -> {
      // Handle response
      io.println("Character: " <> response.character.name)
    }
    Error(err) -> {
      io.println("Error: " <> err)
    }
  }
}
```

### get_characters.gql

Fetches a list of characters.

**Example:**
```gleam
import example/graphql/get_characters

pub fn main() {
  let result = get_characters.get_characters(
    endpoint: "https://rickandmortyapi.com/graphql"
  )

  case result {
    Ok(response) -> {
      // Handle response with list of characters
    }
    Error(err) -> {
      io.println("Error: " <> err)
    }
  }
}
```

## Generate Code

Since this is a separate project, you need to run Squall from the parent directory:

```bash
# From the example directory, go back to parent
cd ..

# Run Squall to generate code
gleam run -m squall generate https://rickandmortyapi.com/graphql

# Go back to example
cd example
```

This will discover the `.gql` files and generate:
- `src/example/graphql/get_character.gleam`
- `src/example/graphql/get_characters.gleam`

Then you can run the example:

```bash
gleam run
```
