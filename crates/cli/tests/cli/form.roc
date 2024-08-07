app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br" }

import pf.Stdin
import pf.Stdout

main =
    Stdout.line! "What's your first name?"
    firstName = Stdin.line!
    Stdout.line! "What's your last name?"
    lastName = Stdin.line!

    Stdout.line "Hi, $(firstName) $(lastName)! 👋"
