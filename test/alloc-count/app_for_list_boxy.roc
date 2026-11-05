app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Regression test for issue 12854: under `--specialize=no` a `for` loop over a
# list advances its iterator by repacking the step thunk's allocation in place,
# so the loop allocates nothing per element. Both lengths depend on the hosted
# input so compile-time evaluation cannot remove the loops, and the allocation
# counts of the short and the long loop must agree: whatever a loop allocates
# to start, it allocates nothing more per element.
run! : Str => Str
run! = |input| {
    seed = Str.count_utf8_bytes(input)
    small = List.repeat(1.U64, seed * 4)
    large = List.repeat(1.U64, seed * 64)

    small_before = Host.alloc_count!()
    var $small_total = 0
    for x in small {
        $small_total = $small_total + x
    }
    small_allocs = Host.alloc_count!() - small_before

    large_before = Host.alloc_count!()
    var $large_total = 0
    for x in large {
        $large_total = $large_total + x
    }
    large_allocs = Host.alloc_count!() - large_before

    expect small_allocs == large_allocs

    "totals: ${$small_total.to_str()} ${$large_total.to_str()}, loop allocations: ${small_allocs.to_str()} ${large_allocs.to_str()}"
}
