app "desugar-bang"
    packages {
        cli: "../basic-cli/platform/main.roc",
    }
    imports [
        cli.Stdout,
        cli.Stderr,
        cli.Task.{ Task },
        cli.Cmd,
    ]
    provides [main] to cli

main = 
    Stdout.line! "Foo"
    Stdout.line! "Bar"