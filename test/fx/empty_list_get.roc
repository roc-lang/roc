app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr
import pf.Host

str : Str -> Str
str = |s| s

main! = || {
    runtime = Host.get_greeting!(Host.new("empty"))
    list = if Str.count_utf8_bytes(runtime) > 0 { [] } else { [1] }
    x = List.get(list, 0)
    if Try.is_ok(x) {
        Stdout.line!("is ok")
    } else {
        Stdout.line!("is err")
    }
}
