interface Route
    exposes [ Handler, get0, get1 ]
    imports [ fx.Effect, Task.{ Task }, Response.{ Response }, pf.UrlParser ]

Handler : [ @Handler (Str -> Result (Task Response []) [ NoMatch ]) ]

get0 : Str, Task Response [] -> Handler
get0 = \part1, task ->
    @Handler \url ->
        if url |> Str.startsWith part1 then
            Ok task

        else
            Err NoMatch

get1 : Str, UrlParser.UrlParser a, Str, (a -> Task Response []) -> Handler
get1 = \part1, parser1, _part2, toTask ->
    @Handler \url ->
        if url |> Str.startsWith part1 then
            # TODO continue parsing the url using parser, part2, and slashes
            when UrlParser.parse url parser1 is
                Ok parsed ->
                    Ok (toTask parsed)

                Err DidNotMatch ->
                    Err NoMatch
        else
            Err NoMatch
