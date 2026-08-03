app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Control for app.roc: the same byte sum written as a `while` loop with manual
# indexing, which is allocation-free today. If this app reports zero loop
# allocations while app.roc reports a nonzero count, the difference is the
# `for` loop's lowering, not the harness.
run! : Str => Str
run! = |input| {
    bytes = Str.to_utf8(input)
    len = List.len(bytes)

    before = Host.alloc_count!()

    var $sum = 0
    var $i = 0
    while $i < len {
        byte = match List.get(bytes, $i) {
            Ok(b) => b
            Err(OutOfBounds) => 0
        }
        $sum = $sum + byte.to_u64()
        $i = $i + 1
    }

    loop_allocs = Host.alloc_count!() - before
    expect loop_allocs == 0

    "sum: ${$sum.to_str()}, loop allocations: ${loop_allocs.to_str()}"
}
