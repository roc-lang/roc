platform "server-side"
    requires {} { app : App state initData }
    exposes []
    packages {}
    provides [main]

import HtmlInternalShared exposing [App]
import HtmlInternalServer exposing [initServerApp]
import Html exposing [renderStatic]
import TotallyNotJson

main : Str, Str -> Result Str Str
main = \initJson, hostJavaScript ->
    initJson
    |> Str.toUtf8
    |> Decode.fromBytes TotallyNotJson.json
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
