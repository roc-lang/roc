app "hello"
    packages { pf:
"https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br"
}
    provides [main] to pf

import pf.Stdout

main =
    Stdout.line "I'm a Roc application!"
