app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    # This has a type error - 5 is a number, not a string
    x : Str
    x = 5
    Stdout.line!("Hello, World!")
}
