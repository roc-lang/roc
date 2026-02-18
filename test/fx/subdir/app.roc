app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout
import pf.Stderr

str : Str -> Str
str = |s| s

main! = || {
    Stdout.line!(str("Hello from stdout!"))
    Stdout.line!(str("Line 1 to stdout"))
    Stderr.line!(str("Line 2 to stderr"))
    Stdout.line!(str("Line 3 to stdout"))
    Stderr.line!(str("Error from stderr!"))
}
