app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main =
    Set.empty
    |> Set.insert "foo"
    |> Set.insert "bar"
    |> Set.insert "foo"
    |> Set.insert "baz"
