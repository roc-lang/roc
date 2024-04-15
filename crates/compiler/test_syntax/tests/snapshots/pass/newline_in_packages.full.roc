app "hello"
    packages { pf:
"https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br"
}
    provides [main] to pf

import pf.Stdout

main =
    Stdout.line "I'm a Roc application!"
