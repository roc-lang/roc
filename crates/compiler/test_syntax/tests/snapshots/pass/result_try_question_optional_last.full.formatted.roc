app [main] {
    cli: platform "",
}

import cli.Stdout
import cli.Task

main =
    "jq --version"
        |> Cmd.new
        |> Cmd.status?
        |> Stdout.line
