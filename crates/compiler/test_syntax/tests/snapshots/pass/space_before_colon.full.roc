app "example"
    packages { pf : "path" }
    provides [ main ] to pf

import pf.Stdout

main = Stdout.line "Hello"
