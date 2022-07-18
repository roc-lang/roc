app "movies"
    packages { pf: "cli-platform/main.roc" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Path.{ Path },
        pf.File.{ FileWriteErr },
        pf.Url.{ Url },
        pf.Http.{ HttpErr },
        pf.Env,
        Encode,
        Json,
    ]
    provides [main] to pf

Movie : {
    title : Str,
    year : U16,
    cast : List Str,
}

movieFromLine : Str -> Result Movie [InvalidLine Str]*
movieFromLine = \line ->
    result =
        fields = Str.split line "|"

        title <- List.get fields 0 |> Result.try
        year <- List.get fields 1 |> Result.try Str.toU16 |> Result.try
        cast <- List.get fields 2 |> Result.try

        Ok { title, year, cast: Str.split cast "," }

    Result.mapErr result \_ -> InvalidLine line

getMovies : Url -> Task (List Movie) (HttpErr [InvalidLine Str]*) [Net]*
getMovies = \url ->
    response <- Http.getUtf8 url |> Task.await

    response
    |> Str.split "\n"
    |> List.mapTry movieFromLine
    |> Task.fromResult

writeOutput : List Movie -> Task {} (FileWriteErr *) [Write [Disk]*]*
writeOutput = \movies ->
    path = Path.fromStr "output.json"

    json = List.keepOks movies \movie ->
        when List.first movie.cast is
            Ok starring if movie.year < 1980 ->
                Ok { title: movie.title, starring }

            _ -> Err {}

    File.write path json Json.toUtf8

main : Task.Task {} [] [Write [Stdout, Disk], Net, Env]
main =
    task =
        apiKey <- Env.varUtf8 "API_KEY" |> Task.await
        url = Url.fromStr "http://localhost:4000/movies?apiKey=\(apiKey)"
        movies <- getMovies url |> Task.await
        writeOutput movies

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Wrote the file!"
            Err (EnvErr _) -> Stderr.line "Could not find API_KEY environment variable"
            Err (HttpErr _) -> Stderr.line "Error reading from URL"
            Err (FileWriteErr _) -> Stderr.line "Error writing to file"
            Err (InvalidLine line) -> Stderr.line "The following line in the response was malformed:\n\(line)"
            Err _ -> Stderr.line "Error!"

# decode movie
expect
    actual = #movieFromLine "title goes here|1234|first star,second star,third star"
        Ok {
            title: "title goes here!",
            year: 1234,
            starring: ["first star", "second star", "third star"]
        }

    actual == Ok {
        title: "title goes here",
        year: 1234,
        starring: ["first star", "second star", "third star"]
    }
