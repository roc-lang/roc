app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

main! = || {
    runtime = Host.get_greeting!(Host.new("guard"))
    value : [Some(U64), None]
    value = Some(Str.count_utf8_bytes(runtime))
    result = match value {
        Some(n) if n > 5 => "big some"
        Some(_) => "small some"
        None => "none"
    }
    Stdout.line!(result)
}
