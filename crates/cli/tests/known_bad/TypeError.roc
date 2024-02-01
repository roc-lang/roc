app "type-error"
    packages { pf: "../../../../examples/cli/false-interpreter/platform/main.roc" }
    provides [main] to pf

import pf.Task exposing [Task]

main : Str -> Task {} []
main = \_ ->
    "this is a string, not a Task {} [] function like the platform expects."
