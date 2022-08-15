app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main =
    Dict.empty
    |> Dict.insert "foo" "bar"
    |> Dict.insert "baz" "blah"
