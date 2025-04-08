app [main] {
    cli: platform "../basic-cli/platform/main.roc",
}

import cli.Stdout

main =
Stdout.line("hello")

