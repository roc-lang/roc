app [main] {
    cli: platform "../basic-cli/platform/main.roc",
}

importcli.Stdout

main =
Stdout.line("hello",)

