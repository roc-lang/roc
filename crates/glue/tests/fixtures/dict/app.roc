app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main =
    Dict.empty
    |> Dict.insert "foo" "this will be overwritten"
    |> Dict.insert "baz" "blah"
    |> Dict.insert "foo" "bar"
