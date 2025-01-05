platform "server-side"
    requires {} { app : App state init_data }
    exposes []
    packages {}
    imports [
        Html.Internal.Shared.{ App },
        Html.Internal.Server.{ init_server_app },
        Html.{ render_static },
        Json,
    ]
    provides [main]

main : Str, Str -> Result Str Str
main = \init_json, host_java_script ->
    init_json
    |> Str.to_utf8
    |> Decode.from_bytes(Json.json)
    |> Result.try(\init_data -> init_server_app(app, init_data, host_java_script))
    |> Result.map(render_static)
    |> Result.map_err(\err ->
        when err is
            TooShort ->
                "JSON initialization data is too short!"

            Leftover(_) ->
                "JSON initialization data is too long!"

            InvalidDocument ->
                "The HTML document must be an <html> tag containing a <body>")
