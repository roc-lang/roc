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
    |> Str.trim
    |> Str.split "\n"
    |> mapTry movieFromLine
    |> Task.fromResult

#writeOutput : List Movie -> Task {} (FileWriteErr (EncodeErr *)) [Write [Disk]*]*
writeOutput : List Movie -> Task {} (FileWriteErr *) [Write [Disk]*]*
writeOutput = \movies ->
    json : List { title : Str, starring : Str }
    json = List.map movies \movie ->
        starring = List.first movie.cast |> Result.withDefault "(no actors)"

        { title: movie.title, starring }

    Path.fromStr "output.json"
    |> write json Json.format

main : Task.Task {} [] [Write [Stdout, Disk], Net, Env]
main =
    task =
        apiKey <- Env.varUtf8 "API_KEY" |> Task.withDefault "" |> Task.await
        url = Url.fromStr "http://localhost:4000/movies?apiKey=\(apiKey)"
        movies <- getMovies url |> Task.await
        writeOutput movies

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Wrote the file!"
            Err (HttpErr _) -> Stderr.line "Error reading from URL"
            Err (FileWriteErr _) -> Stderr.line "Error writing to file"
            Err (InvalidLine line) -> Stderr.line "The following line in the response was malformed:\n\(line)"
            Err _ -> Stderr.line "Error!"

# TODO--------------------------------------------------------------

# TODO annotating this with write : {} throws a compiler panic
write = \path, val, fmt ->
    File.writeBytes path (Encode.toBytes val fmt)

mapTry : List elem, (elem -> Result ok err) -> Result (List ok) err
mapTry = \list, transform ->
    List.walkUntil list (Ok []) \state, elem ->
        when transform elem is
            Ok ok ->
                Result.map state (\elems -> List.append elems ok)
                |> Continue

            Err err -> Break (Err err)

