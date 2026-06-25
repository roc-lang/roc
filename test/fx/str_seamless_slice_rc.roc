app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    source = "RUNTIME_SEAMLESS_SLICE_PREFIX:".concat("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    tail = source.drop_prefix("RUNTIME_SEAMLESS_SLICE_PREFIX:")

    Stdout.line!(tail)
}
