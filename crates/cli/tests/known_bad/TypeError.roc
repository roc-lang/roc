app [main] { pf: platform "../../../../examples/cli/false-interpreter/platform/main.roc" }

main : Str -> Task {} []
main = \_ ->
    "this is a string, not a Task {} [] function like the platform expects."
