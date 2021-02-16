app "closure-borrowed"
    packages { base: "platform" }
    imports [base.Task]
    provides [ main ] to base

# see https://github.com/rtfeldman/roc/issues/985

main : Task.Task {} []
main =
    Task.succeed (foo toUnitBorrowed "a long string such that it's malloced")
        |> Task.map (\_ -> {})

toUnitBorrowed = \x -> Str.countGraphemes x

foo = \f, x -> f x
