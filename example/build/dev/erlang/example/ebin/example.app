{application, example, [
    {vsn, "0.1.0"},
    {applications, [gleam_http,
                    gleam_httpc,
                    gleam_json,
                    gleam_stdlib,
                    squall]},
    {description, "Example project using Squall with Rick and Morty API"},
    {modules, [example,
               graphql@get_character,
               graphql@get_characters]},
    {registered, []}
]}.
