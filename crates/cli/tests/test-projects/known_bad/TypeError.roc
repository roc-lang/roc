app [main] { pf: platform "../false-interpreter/platform/main.roc" }

main : Str -> List ()
main = \_ ->
    "this is a string, not a List () function like the platform expects."
