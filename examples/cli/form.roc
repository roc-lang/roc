app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }

import pf.Stdin
import pf.Stdout
import pf.Task exposing [await, Task]

main : Task {} I32
main =
    _ <- await (Stdout.line "What's your first name?")
    firstName <- await Stdin.line

    _ <- await (Stdout.line "What's your last name?")
    lastName <- await Stdin.line

    Stdout.line "Hi, $(unwrap firstName) $(unwrap lastName)! 👋"

unwrap : [Input Str, End] -> Str
unwrap = \input ->
    when input is
        Input line -> line
        End -> "Received end of input (EOF)."
