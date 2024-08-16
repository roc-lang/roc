app [main] {
    cli: platform "",
}

main =
    "jq --version"
        |> Cmd.new
        |> Cmd.status
        |> Task.mapErr! UnableToCheckJQVersion
