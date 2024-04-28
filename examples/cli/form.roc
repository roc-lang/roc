app "form"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Task]
    provides [main] to pf

main =
    Stdout.line! "What's your first name?"
    firstName = Stdin.line!

    Stdout.line! "What's your last name?"
    lastName = Stdin.line!

    Stdout.line "Hi, $(firstName) $(lastName)! 👋"
