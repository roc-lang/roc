app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost
import Issue13356Parity
import Issue13356Loop
import Issue13356SelfTail

# repro for https://github.com/roc-lang/roc/issues/13356
# Ordinary calls from this module to functions in cycles of tail calls
# another module defines: a closed-type pair, a pair with a generic
# parameter, and a function that tail-calls itself. The host's input is
# 21 bytes, so the counts are only known at run time.
main! = || {
    bytes = Str.count_utf8_bytes(FallibleHost.json_input!({}))
    parity = Issue13356Parity.even(60 + bytes, 0)
    loop = Issue13356Loop.ping({ a: 1, b: "r" }, bytes - 16, ">")
    self_tail = Issue13356SelfTail.nt(bytes - 19, "a")
    "${parity.to_str()} ${loop} ${self_tail(3).to_str()}"
}
