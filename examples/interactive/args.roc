app "args"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task, pf.Arg]
    provides [main] to pf

main : List Str -> Task.Task {} [] [Write [Stdout]]
main = \args ->
    parser =
        Arg.succeed (\mode -> mode)
        |> Arg.apply (Arg.str { long: "mode", help: Some "the foo flag" })

    when Arg.parse parser args is
        Ok mode ->
            Stdout.line "You chose mode \(mode)"
        Err (MissingRequiredArg arg) ->
            Stdout.line """The arg "\(arg)" was not provided!"""
        Err (WrongType { arg, expected: _ }) ->
            Stdout.line """The arg "\(arg)" must have a type"""
