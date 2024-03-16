app "closure"
    packages { pf: "platform/main.roc" }
    provides [main] to pf

import pf.Task

# see https://github.com/roc-lang/roc/issues/985
main : Task.Task {} []
main = closure1 {}
# |> Task.after (\_ -> closure2 {})
# |> Task.after (\_ -> closure3 {})
# |> Task.after (\_ -> closure4 {})
# ---
closure1 : {} -> Task.Task {} []
closure1 = \_ ->
    Task.succeed (foo toUnitBorrowed "a long string such that it's malloced")
    |> Task.map \_ -> {}

toUnitBorrowed = \x -> Str.countUtf8Bytes x

foo = \f, x -> f x

# ---
# closure2 : {} -> Task.Task {} []
# closure2 = \_ ->
#     x : Str
#     x = "a long string such that it's malloced"
#
#     Task.succeed {}
#         |> Task.map (\_ -> x)
#         |> Task.map toUnit
#
# toUnit = \_ -> {}
#
# # ---
# closure3 : {} -> Task.Task {} []
# closure3 = \_ ->
#     x : Str
#     x = "a long string such that it's malloced"
#
#     Task.succeed {}
#         |> Task.after (\_ -> Task.succeed x |> Task.map (\_ -> {}))
#
# # ---
# closure4 : {} -> Task.Task {} []
# closure4 = \_ ->
#     x : Str
#     x = "a long string such that it's malloced"
#
#     Task.succeed {}
#         |> Task.after (\_ -> Task.succeed x)
#         |> Task.map (\_ -> {})
