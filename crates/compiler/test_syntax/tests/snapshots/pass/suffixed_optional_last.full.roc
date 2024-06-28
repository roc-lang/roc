app ""
    packages {
        cli: "",
    }
    imports [cli.Stdout, cli.Task]
    provides [main] to cli

main = 
    "jq --version"
        |> Cmd.new
        |> Cmd.status
        |> Task.mapErr! UnableToCheckJQVersion


    "jq --version"
        |> Cmd.new
        |> Cmd.status?
        |> Stdout.line
