app "args"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task, pf.Arg]
    provides [main] to pf

main : List Str -> Task.Task {} [] [Write [Stdout]]
main = \args ->
    parser =
        Arg.succeed (\mode -> mode)
        |> Arg.apply (Arg.str { long: "mode", help: Some "the mode flag" })

    when Arg.parseFormatted parser args is
        Ok mode ->
            Stdout.line "You chose mode \(mode)"
        Err helpMenu ->
            Stdout.line helpMenu
