interface Route
    exposes [ Handler, get0, get1, runHandlers ]
    imports [ fx.Effect, Task.{ Task }, Response.{ Response }, pf.UrlParser ]

Handler : [ @Handler (Str -> Result (Task Response []) [ NoMatch ]) ]

runHandlers : Str, List Handler -> Task Response []
runHandlers = \url, handlers -> 
    walker = \@Handler handler, accum ->
        when handler url is
            Ok task -> Stop (Ok task)
            Err NoMatch -> Continue accum 

    List.walkUntil handlers walker (Err {})
        |> Result.withDefault (Task.succeed (Response.status 404 "not found"))
    
    

get0 : Str, Task Response [] -> Handler
get0 = \part1, task ->
    @Handler \url ->
        parts = Str.split url "/"

        if List.len parts == 2 then 
            when List.get parts 1 is
                Ok part if part == part1 ->
                    Ok task

                _ ->
                    Err NoMatch

        else
            Err NoMatch

get1 : Str, UrlParser.UrlParser a, (a -> Task Response []) -> Handler
get1 = \part1, parser1, toTask ->
    @Handler \url ->
        parts = Str.split url "/"

        when Pair (List.get parts 1) (List.get parts 2) is
            Pair (Ok a) (Ok b) if a == part1 ->
                when UrlParser.parse b parser1 is
                    Ok parsed ->
                        Ok (toTask parsed)

                    Err DidNotMatch ->
                        Err NoMatch

            Pair _ _ -> 
                Err NoMatch
