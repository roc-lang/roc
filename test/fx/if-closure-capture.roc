app [main!] { pf: platform "./platform/main.roc" }

import pf.Host

main! = || {
    runtime = Host.get_greeting!(Host.new("capture"))
    if Str.count_utf8_bytes(runtime) > 0 {
        x = 0
        _y = x
    }
}
