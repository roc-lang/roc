app "test"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnYRvdIwDJd0Vot7g-xbWeo0IH9aHuwi4.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    # Type error: adding a number to a string
    x = 5 + "hello"

    # Undefined variable
    y = undefinedVariable

    # Type mismatch in pattern matching
    when x is
        "string" -> Stdout.line "This won't work"
        42 -> Stdout.line "Neither will this"
        _ -> Stdout.line "Error"
