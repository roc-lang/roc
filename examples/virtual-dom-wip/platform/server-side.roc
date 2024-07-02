platform "server-side"
    requires {} { app : App state initData }
    exposes []
    packages {}
    imports [
        Html.Internal.Shared.{ App },
        Html.Internal.Server.{ initServerApp },
        Html.{ renderStatic },
        Json,
    ]
    provides [main]

main : Str, Str -> Result Str Str
main = \initJson, hostJavaScript ->
    initJson
    |> Str.toUtf8
    |> Decode.fromBytes Json.json
    |> Result.try \initData -> initServerApp app initData hostJavaScript
    |> Result.map renderStatic
    |> Result.mapErr \err ->
        when err is
            TooShort ->
                "JSON initialization data is too short!"

            Leftover _ ->
                "JSON initialization data is too long!"

            InvalidDocument ->
                "The HTML document must be an <html> tag containing a <body>"
