app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

main! = || {
    runtime = Host.get_greeting!(Host.new("list"))
    my_list = [8]
    index = if Str.count_utf8_bytes(runtime) > 0 { 0 } else { 1 }
    foo = my_list.get(index)
    if Try.is_ok(foo) {
        Stdout.line!("is ok")
    } else {
        Stdout.line!("is err")
    }
}
