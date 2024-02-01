app "http"
    packages { pf: "https://github.com/roc-lang/basic-webserver/releases/download/0.1/dCL3KsovvV-8A5D_W_0X_abynkcRcoAngsgF0xtvQsk.tar.br" }
    provides [main] to pf

import pf.Stdout
import pf.Stderr
import pf.Task exposing [Task]
import pf.Http exposing [Request, Response]
import pf.Utc
import pf.Env

main : Request -> Task Response []
main = \req ->

    handleReq =
        # Log the date, time, method, and url to stdout
        {} <- logRequest req |> Task.await

        # Read environment variable
        url <- readUrlEnv "TARGET_URL" |> Task.await

        # Fetch the Roc website
        content <- fetchContent url |> Task.await

        # Respond with the website content
        respond 200 content

    # Handle any application errors
    handleReq |> Task.onErr handleErr

AppError : [
    EnvURLNotFound,
    HttpError Http.Error,
]

logRequest : Request -> Task {} AppError
logRequest = \req ->
    dateTime <- Utc.now |> Task.map Utc.toIso8601Str |> Task.await

    Stdout.line "\(dateTime) \(Http.methodToStr req.method) \(req.url)"

readUrlEnv : Str -> Task Str AppError
readUrlEnv = \target ->
    Env.var target
    |> Task.mapErr \_ -> EnvURLNotFound

fetchContent : Str -> Task Str AppError
fetchContent = \url ->
    Http.getUtf8 url
    |> Task.mapErr \err -> HttpError err

handleErr : AppError -> Task Response []
handleErr = \err ->

    # Build error message
    message =
        when err is
            EnvURLNotFound -> "TARGET_URL environment variable not set"
            HttpError _ -> "Http error fetching content"

    # Log error to stderr
    {} <- Stderr.line "Internal Server Error: \(message)" |> Task.await
    _ <- Stderr.flush |> Task.attempt

    # Respond with Http 500 Error
    Task.ok {
        status: 500,
        headers: [
            { name: "Content-Type", value: Str.toUtf8 "text/html; charset=utf-8" },
        ],
        body: Str.toUtf8 "Error 500 Internal Server Error\n",
    }

# Respond with the given status code and body
respond : U16, Str -> Task Response AppError
respond = \code, body ->
    Task.ok {
        status: code,
        headers: [
            { name: "Content-Type", value: Str.toUtf8 "text/html; charset=utf-8" },
        ],
        body: Str.toUtf8 body,
    }
