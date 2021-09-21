interface Route
    exposes [ Handler, get0, get1 ]
    imports [ fx.Effect, Task.{ Task } ]

Handler : [ @Handler (Str -> Result (Task Response []) [ NoMatch ]) ]

get0 : Str, Task Response [] -> Handler

get1 : Str, UrlParser a, Str, (a -> Task Response []) -> Handler
get1 = \part1, parser1, part2, toTask ->
    \url ->
        if url |> Str.startsWith part1 then
            # TODO continue parsing the url using parser, part2, and slashes
            when UrlParser.parse url parser is
                Ok parsed ->
                    Ok (toTask parsed)

                Err DidNotMatch ->
                    Err NoMatch
        else
            Err NoMatch
