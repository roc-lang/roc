app "old-app-header"
    packages {
        cli: "../basic-cli/platform/main.roc",
    }
    imports [
        cli.Stdout,
    ]
    provides [main] to cli

main =
    Stdout.line "hello"

