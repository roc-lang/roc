app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    name : Str
    name = "World"
    Stdout.line!("Hello, ${name}!")
}
