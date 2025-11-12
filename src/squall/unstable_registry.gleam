import gleam/dict.{type Dict}

/// Query metadata stored in the registry
pub type QueryMeta {
  QueryMeta(query: String, module_path: String)
}

/// Query registry that maps query names to their metadata
pub type Registry {
  Registry(queries: Dict(String, QueryMeta))
}

/// Create a new empty registry
pub fn new() -> Registry {
  Registry(queries: dict.new())
}

/// Register a query with its metadata
pub fn register(
  registry: Registry,
  query_name: String,
  query: String,
  module_path: String,
) -> Registry {
  let meta = QueryMeta(query: query, module_path: module_path)
  Registry(queries: dict.insert(registry.queries, query_name, meta))
}

/// Get query metadata by name
pub fn get(registry: Registry, query_name: String) -> Result(QueryMeta, Nil) {
  dict.get(registry.queries, query_name)
}
