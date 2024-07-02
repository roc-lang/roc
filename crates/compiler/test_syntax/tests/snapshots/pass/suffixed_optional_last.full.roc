app ""
    packages {
        cli: "",
    }
    imports []
    provides [main] to cli

main = 
    "jq --version"
        |> Cmd.new
        |> Cmd.status
        |> Task.mapErr! UnableToCheckJQVersion
