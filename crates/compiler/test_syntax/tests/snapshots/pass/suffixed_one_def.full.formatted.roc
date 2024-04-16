app "desugar-bang"
    packages {
        cli: "../basic-cli/platform/main.roc",
    }
    imports [
        cli.Stdout,
    ]
    provides [main] to cli

main =
    # is this a valid statement?
    "Foo" |> A.x!
    # what about this?
    "Bar"
        |> B.y!
            { config: "config" }

    C.z "Bar"
