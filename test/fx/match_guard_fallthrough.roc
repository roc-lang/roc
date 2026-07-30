app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

main! = || {
    runtime = Host.get_greeting!(Host.new("guard"))
    result = match Str.count_utf8_bytes(runtime) {
        x if x > 100 => "big"
        _ => "small"
    }
    Stdout.line!(result)
}
