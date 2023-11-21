app "form"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, Task }]
    provides [main] to pf

main : Task {} I32
main =
    _ <- await (Stdout.line "What's your first name?")
    firstName <- await Stdin.line

    _ <- await (Stdout.line "What's your last name?")
    lastName <- await Stdin.line

    Stdout.line "Hi, \(unwrap firstName) \(unwrap lastName)! 👋"

unwrap : [Input Str, End] -> Str
unwrap = \input ->
    when input is
        Input line -> line
        End -> "Received end of input (EOF)."
