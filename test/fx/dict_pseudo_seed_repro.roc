app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

main! = || {
    runtime = Host.get_greeting!(Host.new("dict"))
    key = if Str.count_utf8_bytes(runtime) > 0 { "a" } else { "missing" }
    dict = Dict.single("a", "b")

    match Dict.get(dict, key) {
        Ok(value) => Stdout.line!(value)
        Err(_) => Stdout.line!("missing")
    }
}
