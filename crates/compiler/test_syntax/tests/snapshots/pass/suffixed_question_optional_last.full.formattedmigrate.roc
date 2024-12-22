app [main] {
    cli: platform "",
}

main =
    "jq --version"
    |> Cmd.new
    |> Cmd.status
    |> Result.map_err? UnableToCheckJQVersion
