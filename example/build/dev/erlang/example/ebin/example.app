{application, example, [
    {vsn, "0.1.0"},
    {applications, [gleam_http,
                    gleam_httpc,
                    gleam_json,
                    gleam_stdlib,
                    squall]},
    {description, "Example project using Squall with Rick and Morty API"},
    {modules, [example_with_vars,
               graphql@multi_query_with_vars]},
    {registered, []}
]}.
