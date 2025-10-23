# Claude Context: Squall

## Project Overview

**Squall** is a type-safe GraphQL client generator for Gleam, inspired by [squirrel](https://github.com/giacomocavalieri/squirrel). It follows a convention-over-configuration approach where developers write `.gql` files in `src/**/graphql/` directories, and Squall generates fully type-safe Gleam code with proper types and JSON decoders.

## Architecture

### Core Philosophy

1. **Convention over Configuration**: No config files, just drop `.gql` files in the right place
2. **Type Safety First**: All generated code is fully typed based on GraphQL schema
3. **Test-Driven Development**: Every feature has tests written first
4. **Modular Design**: Clear separation of concerns across modules

### Module Breakdown

```
src/squall/internal/
├── error.gleam           # Comprehensive error types
├── discovery.gleam       # File discovery and validation
├── parser.gleam          # GraphQL lexer + recursive descent parser
├── schema.gleam          # GraphQL schema introspection
├── type_mapping.gleam    # GraphQL to Gleam type conversion
└── codegen.gleam         # Code generation with glam
```

### Data Flow

```
.gql files → discovery → parser → type_mapping → codegen → .gleam files
                ↓
         schema (from endpoint introspection)
```

## Key Design Patterns

### 1. Error Handling

All modules use a central `Error` type in `error.gleam`. Errors are:
- Descriptive with file paths and line numbers where applicable
- Converted to strings via `error.to_string()` for CLI output
- Propagated using `Result(T, Error)`

### 2. Type Safety

GraphQL types map to Gleam types:
- `String`, `Int`, `Float`, `Boolean`, `ID` → Built-in types
- `Type` (nullable) → `Option(Type)`
- `Type!` (non-null) → `Type`
- `[Type]` → `List(Type)`
- Custom objects → Custom Gleam types

### 3. Testing Strategy

**TDD Approach:**
1. Write test first (in `test/` directory)
2. Run test (it should fail)
3. Implement feature
4. Run test (it should pass)
5. Refactor if needed

**Test Types:**
- **Unit tests**: Each module has its own test file
- **Snapshot tests**: Using `birdie` for code generation validation
- **Integration tests**: Test file embedded in `test/fixtures/graphql/`

### 4. Code Generation

Uses the `glam` library for pretty-printing. Generated code includes:
- Type definitions (custom types for responses)
- JSON decoders (using `gleam/dynamic`)
- HTTP client functions (using `gleam_http` + `gleam_httpc`)

## File Conventions

### GraphQL Files

- **Location**: `src/**/graphql/*.gql`
- **Naming**: Filename becomes function name (must be valid Gleam identifier)
  - ✅ `get_user.gql` → `get_user()`
  - ✅ `create_post.gql` → `create_post()`
  - ❌ `get-user.gql` (hyphens not allowed)
  - ❌ `123user.gql` (can't start with number)

### Generated Files

- **Output**: Same directory as `.gql` file, same name with `.gleam` extension
- **Pattern**: `src/**/graphql/operation_name.gql` → `src/**/graphql/operation_name.gleam`

## Development Workflow

### Adding a New Feature

1. **Write test first** in appropriate test file
2. **Run tests**: `gleam test` (should fail)
3. **Implement feature** in appropriate module
4. **Run tests**: `gleam test` (should pass)
5. **Update snapshots if needed**: `BIRDIE_ACCEPT=1 gleam test`

### Testing

```bash
# Run all tests
gleam test

# Accept birdie snapshots
BIRDIE_ACCEPT=1 gleam test

# Build the project
gleam build

# Run CLI
gleam run -m squall generate <endpoint>
```

## Important Implementation Details

### Parser (`parser.gleam`)

- **Two-phase**: Lexer (tokenization) → Parser (AST building)
- **Recursive descent**: Standard parsing technique
- **AST Types**: `Operation`, `Selection`, `TypeRef`, `Variable`, `Value`
- **Handles**: Queries, Mutations, Subscriptions, Variables, Arguments, Nested selections

### Schema (`schema.gleam`)

- **Introspection**: Parses GraphQL introspection JSON
- **Types**: `Type`, `Field`, `InputValue`, `TypeRef`
- **Schema**: Contains query/mutation/subscription types + all schema types
- **Validation**: Checks for required fields, proper structure

### Type Mapping (`type_mapping.gleam`)

- **GleamType**: Internal representation of Gleam types
- **Conversion**: GraphQL TypeRef → GleamType
- **Nullable handling**: Automatically wraps nullable types in `Option`
- **Lists**: Properly handles `[Type]`, `[Type!]`, `[Type]!`, `[Type!]!`

### Code Generation (`codegen.gleam`)

- **Response Types**: Generated for each operation
- **Decoders**: JSON decoders using `gleam/dynamic`
- **Functions**: Type-safe functions with proper parameters
- **Imports**: Automatically includes required imports

## Common Tasks

### Add Support for a New GraphQL Feature

Example: Adding fragment support

1. **Update Parser**:
   - Add fragment tokens to lexer
   - Add fragment AST types
   - Add fragment parsing logic

2. **Update Code Generation**:
   - Handle fragments in selection sets
   - Generate fragment helper functions

3. **Write Tests**:
   ```gleam
   // In parser_test.gleam
   pub fn parse_fragment_test() {
     let source = "fragment UserFields on User { id name }"
     let result = parser.parse(source)
     should.be_ok(result)
   }
   ```

4. **Add Snapshot Test**:
   ```gleam
   // In codegen_test.gleam
   pub fn generate_with_fragments_test() {
     // Setup and generate code
     code |> birdie.snap(title: "Query with fragments")
   }
   ```

### Fix a Type Mapping Issue

1. **Locate the issue** in `type_mapping.gleam`
2. **Write a failing test** in `type_mapping_test.gleam`
3. **Fix the mapping** logic
4. **Verify test passes**

### Update Generated Code Format

1. **Modify** `codegen.gleam`
2. **Run tests**: They will fail with snapshot differences
3. **Review the diff** to ensure it's correct
4. **Accept snapshots**: `BIRDIE_ACCEPT=1 gleam test`

## Known Limitations

1. **HTTP Client**: Currently returns an error - needs full implementation
2. **Fragments**: Not yet implemented
3. **Directives**: Not yet handled (@include, @skip, etc.)
4. **Custom Scalars**: All map to String by default
5. **Introspection**: Doesn't handle deeply nested types beyond 5 levels

## Code Style

### Gleam Conventions

- **Functions**: `snake_case`
- **Types**: `PascalCase`
- **Constants**: `snake_case`
- **Pipes**: Use `|>` for chaining
- **Pattern Matching**: Prefer `case` over nested `if`

### Project Conventions

- **Naming**: Be descriptive, avoid abbreviations
- **Comments**: Explain "why" not "what"
- **Error Messages**: Include file paths and suggestions when possible
- **Tests**: One test per feature, descriptive test names

## Troubleshooting

### Tests Not Running

- Check `test/squall_test.gleam` - it manually calls all tests
- Ensure test files don't have their own `main()` functions
- Test functions must end with `_test`

### Snapshot Tests Failing

- Run `BIRDIE_ACCEPT=1 gleam test` to accept new snapshots
- Review `.new` files in `birdie_snapshots/` before accepting
- Snapshots are in `birdie_snapshots/*.accepted`

### Parser Errors

- Check token definitions in `parser.gleam`
- Ensure lexer handles all GraphQL syntax
- Test with simple queries first, then complex ones

## Future Improvements

### High Priority

- [ ] Implement full HTTP client (replace stub in `make_graphql_request`)
- [ ] Add fragment support
- [ ] Handle GraphQL directives
- [ ] Custom scalar type mapping

### Medium Priority

- [ ] Multiple endpoint support
- [ ] Watch mode for development
- [ ] Better error messages with source code snippets
- [ ] Cache schema introspection results

### Low Priority

- [ ] IDE integration
- [ ] GraphQL validation against schema
- [ ] Performance optimizations
- [ ] Parallel file processing

## References

- **Gleam**: https://gleam.run/
- **Squirrel**: https://github.com/giacomocavalieri/squirrel
- **GraphQL**: https://graphql.org/
- **Rick and Morty API**: https://rickandmortyapi.com/graphql (used for testing)

## Questions?

When working on this project:

1. **Follow TDD**: Always write tests first
2. **Check existing patterns**: Look at similar features for guidance
3. **Update tests**: If changing behavior, update snapshots
4. **Ask for context**: Use this document as a starting point

Happy coding! 🌊
