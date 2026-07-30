app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

str : Str -> Str
str = |s| s

main! = || {
    runtime = Host.get_greeting!(Host.new("match"))
    x = match runtime {
        _ => str("0")
    }
    Stdout.line!(x)
}
