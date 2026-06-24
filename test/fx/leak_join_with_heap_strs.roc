app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Regression test for the lld-COFF &strDecref misresolution bug: Str.join_with
# consumes a unique List(Str), and its heap element strings must be decref'd.
# Every string is derived from stdin so it exists only at runtime (literals can
# be static rc=0 constants), and is >= 24 bytes so it is heap-allocated rather
# than small-string-optimized.

heap_str : Str, Str -> Str
heap_str = |seed, tag| Str.repeat(Str.concat(seed, tag), 4)

main! = || {
    seed = Stdin.line!()
    a = heap_str(seed, "-alpha")
    b = heap_str(seed, "-beta-")
    c = heap_str(seed, "-gamma")
    joined = Str.join_with([a, b, c], ",")
    Stdout.line!("joined bytes: ${Str.count_utf8_bytes(joined).to_str()}")
}
