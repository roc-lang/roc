platform "server-side"
    requires {} { app : App state initData }
    exposes []
    packages {}
    imports [
        Html.Internal.{ App, initServerApp },
        Html.{ renderStatic },
        Json,
    ]
    provides [main]

main : Str -> Result Str Str
main = \initJson ->
    initJson
    |> Str.toUtf8
    |> Decode.fromBytes Json.fromUtf8
    |> Result.try \initData -> initServerApp initData app
    |> Result.map renderStatic
    |> Result.mapErr \err ->
        when err is
            TooShort ->
                "JSON initialization data is too short!"

            Leftover _ ->
                "JSON initialization data is too long!"

            InvalidDocument ->
                "The HTML document must be an <html> tag containing a <body>"
