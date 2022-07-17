app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports [Json, Encode]
    provides [main] to pf

main =
    ["Hello, world!"]
    |> mapTry (\str -> Ok str)
    |> Result.after List.first
    |> Result.withDefault "hi"

mapTry : List elem, (elem -> Result ok err) -> Result (List ok) err
mapTry = \list, transform ->
    List.walkUntil list (Ok []) \state, elem ->
        when transform elem is
            Ok ok ->
                Result.map state (\elems -> List.append elems ok)
                |> Continue

            Err err -> Break (Err err)

