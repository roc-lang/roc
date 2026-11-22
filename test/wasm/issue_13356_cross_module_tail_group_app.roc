app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost
import Issue13356Parity
import Issue13356Loop

# repro for https://github.com/roc-lang/roc/issues/13356
# Ordinary calls from this module to functions in cycles of tail calls
# another module defines: a closed-type pair, and a pair with a generic
# parameter. The host's input is 21 bytes, so the counts are only known
# at run time.
main! = || {
    bytes = Str.count_utf8_bytes(FallibleHost.json_input!({}))
    parity = Issue13356Parity.even(60 + bytes, 0)
    loop = Issue13356Loop.ping({ a: 1, b: "r" }, bytes - 16, ">")
    "${parity.to_str()} ${loop}"
}
