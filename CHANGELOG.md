# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2025-11-12

### Added

- **Relay-style cache normalization support** via new `unstable-cache` command
  - Injects `__typename` into queries for cache entity identification
  - Generates cache registry with all queries for centralized cache management
  - Extracts GraphQL queries from doc comments in `.gleam` files
  - Outputs generated types to `src/generated/queries/` by default
- **Enum support**
  - Generates enum type definitions with PascalCase variants
  - Creates `enum_to_string()` converter functions for serialization
  - Generates `enum_decoder()` functions for deserialization
  - Support for enums in response types, variables, and InputObjects
  - Support for optional enums (`Option(EnumType)`)
  - Support for lists of enums (`List(EnumType)`)

### Fixed

- **Enum serialization in response types** - Response serializers now correctly convert enum values to strings using generated `enum_to_string()` functions instead of attempting direct string conversion
- **Enum serialization in optional fields** - Optional enum fields now properly serialize with `json.nullable` wrapper
- **Enum serialization in lists** - Lists of enums now correctly serialize each enum value to a string

### Changed

- **Removed `output_path` parameter from `unstable-cache` command** - Output path is now fixed at `src/generated` for consistency

### Commands

#### New: `unstable-cache`

```bash
gleam run -m squall unstable-cache <endpoint>
```

Extracts GraphQL queries from doc comments and generates:
- Type-safe Gleam functions for each query at `src/generated/queries/`
- Cache registry initialization module at `src/generated/queries.gleam`
- Automatic `__typename` injection for cache normalization

## [1.0.1] - 2025-11-10

### Added
- Lustre example application
- Fragment support in queries

### Changed
- Updated description
- Updated README
- Removed unused dependencies

## [1.0.0] - 2025-11-07

Initial release with core functionality:
- Sans-IO GraphQL client generation
- Support for both Erlang and JavaScript targets
- GraphQL query, mutation, and subscription support
- Variable and argument handling
- InputObject type support
- JSON scalar type mapping
- Response serializers
- Reserved keyword sanitization
