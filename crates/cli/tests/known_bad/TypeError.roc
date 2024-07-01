app "type-error"
    packages { pf: "../../../../examples/cli/false-interpreter/platform/main.roc" }
    imports []
    provides [main] to pf

main : Str -> Task {} []
main = \_ ->
    "this is a string, not a Task {} [] function like the platform expects."
