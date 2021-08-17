app "test-example"
    packages { base: "../hello-world/platform" }
    imports [ Test ]
    provides [ main ] to base

test1 = { name: "1 + 1 == 2", test: 1 + 1 == 2 }

test2 = { name: "2 * 3 == 6", test: 2 * 3 == 6 }

main : Str
main =
    [ test1, test2 ]
        |> Test.run "Example tests"
