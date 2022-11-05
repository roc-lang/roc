platform "static-site-gen"
    requires {} { main : App state initData }
    exposes []
    packages {}
    imports [
        Html.Internal.{ App, initServerApp },
        Html.{ renderStatic },
        Json,
    ]
    provides [mainForHost]

mainForHost : Str -> Result Str Str
mainForHost = \initJson ->
    initJson
    |> Str.toUtf8
    |> Decode.fromBytes Json.fromUtf8
    |> Result.try \initData -> initServerApp initData main
    |> Result.map renderStatic
    |> Result.mapErr \err ->
        when err is
            TooShort ->
                "JSON initialization data is too short!"

            Leftover _ ->
                "JSON initialization data is too long!"

            InvalidDocument ->
                "The HTML document must be an <html> tag containing a <body>"

            MissingHtmlIds _ ->
                "Some dynamic views have no corresponding HTML IDs in the static document"
