app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    provides [main] to pf

import pf.Stdout

main =
    Stdout.line "Hello, World!"
