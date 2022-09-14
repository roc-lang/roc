app "args"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task]
    provides [main] to pf

main : List Str -> Task.Task {} [] [Write [Stdout]]
main = \args ->
    _ <- Task.await (Stdout.line "Here are the args I got from the command line:")
    joinedArgs = Str.joinWith args " "

    Stdout.line joinedArgs
