app "desugar-bang"
    packages {
        cli: "../basic-cli/platform/main.roc",
    }
    imports [
        cli.Stdout,
    ]
    provides [main] to cli

main =
    Stdout.line! "Foo"

    "Bar" 
    |> Stdout.line 
        