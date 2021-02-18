app "closure3"
    packages { base: "platform" }
    imports [base.Task]
    provides [ main ] to base

# see https://github.com/rtfeldman/roc/issues/985

main : Task.Task {} []
main =
    x : Str
    x = "a long string such that it's malloced"

    Task.succeed {}
        |> Task.after (\_ -> Task.succeed x |> Task.map (\_ -> {}))
        
