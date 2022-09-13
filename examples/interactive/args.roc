app "args"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task, pf.Arg]
    provides [main] to pf

main : List Str -> Task.Task {} [] [Write [Stdout]]
main = \args ->
    parser =
        Arg.choice [
            Arg.succeed (\s -> Exclaim s)
            |> Arg.withParser
                (
                    Arg.str {
                        long: "string",
                        short: Some "s",
                        help: Some "the string to exclaim",
                    }
                )
            |> Arg.subCommand "exclaim",
            Arg.succeed (\name -> \greeting -> Greet { name, greeting })
            |> Arg.withParser
                (
                    Arg.str {
                        long: "name",
                        help: Some "the name of the individual to greet",
                    }
                )
            |> Arg.withParser
                (
                    Arg.str {
                        long: "greeting",
                        short: Some "g",
                        help: Some "the greeting to use",
                    }
                )
            |> Arg.subCommand "greet",
        ]
        |> Arg.program { name: "args-example", help: "An example of the CLI platform argument parser" }

    when Arg.parseFormatted parser args is
        Ok (Exclaim s) ->
            Stdout.line "\(s)!"

        Ok (Greet { name, greeting }) ->
            Stdout.line "\(greeting), \(name)"

        Err helpMenu ->
            Stdout.line helpMenu
