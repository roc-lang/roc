app [run!] {
    lib: "./lib/main.roc",
    pf: platform "../../alloc-count/platform/main.roc",
}

import pf.Host
import Fmt

# https://github.com/roc-lang/roc/issues/13081
# Encoding more items must grow the unique buffer geometrically, not copy it
# per callback. Runtime input prevents compile-time evaluation of the encodes.
run! : Str => Str
run! = |input| {
    seed = input.count_utf8_bytes()
    small = build_items(seed * 4)
    large = build_items(seed * 64)

    before_small = Host.alloc_count!()
    small_bytes = Fmt.encode({ marker: seed * 4, values: small })
    small_allocs = Host.alloc_count!() - before_small
    before_large = Host.alloc_count!()
    large_bytes = Fmt.encode({ marker: seed * 64, values: large })
    large_allocs = Host.alloc_count!() - before_large

    # Two container headers, two six-byte field names, and the marker byte.
    correct = small_bytes.len() == seed * 4 + 15 and
        large_bytes.len() == seed * 64 + 15 and
        small_bytes.last() == Ok((seed * 4 - 1).to_u8_wrap()) and
        large_bytes.last() == Ok((seed * 64 - 1).to_u8_wrap())
    amortized = large_allocs <= small_allocs + 32

    # The CLI harness asserts these results; optimized builds erase `expect`.
    "issue 13081: correct ${Str.inspect(correct)}, amortized ${Str.inspect(amortized)}, allocations ${small_allocs.to_str()} ${large_allocs.to_str()}"
}

build_items : U64 -> List(U64)
build_items = |count| {
    var $items = []
    for i in 0.U64.until(count) {
        $items = $items.append(i)
    }
    $items
}
