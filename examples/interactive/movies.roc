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
    Ok { title: line, year: 0, cast: [] }

getMovies : Url -> Task (List Movie) (HttpErr [InvalidLine Str]*) [Net]*
getMovies = \url ->
    response <- Http.getUtf8 url |> Task.await

    response
    |> Str.trim
    |> Str.split "\n"
    |> mapTry movieFromLine
    |> Task.fromResult

main : Task.Task {} [] [Write [Stdout, Disk], Net, Env]
main =
    Task.attempt (getMovies (Url.fromStr "http://localhost:4000/movies")) \result ->
        when result is
            Ok _ -> Stdout.line "Wrote the file!"
            Err _ -> Stderr.line "Error!"

# TODO--------------------------------------------------------------

# TODO annotating this with write : {} throws a compiler panic
write = \path, val, fmt ->
    File.writeBytes path (Encode.toBytes val fmt)

try : Result a err, (a -> Result b err) -> Result b err
try = Result.after

mapTry : List elem, (elem -> Result ok err) -> Result (List ok) err
mapTry = \list, transform ->
    List.walkUntil list (Ok []) \state, elem ->
        when transform elem is
            Ok ok ->
                Result.map state (\elems -> List.append elems ok)
                |> Continue

            Err err -> Break (Err err)

