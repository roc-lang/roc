app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Issue 12854's adapter chains under `--specialize=no`: a `for` loop over a
# chain of iterator adapters steps each adapter's step closure, and an
# adapter's step builds the next one around its upstream's, so these loops
# allocate per step. Every length depends on the hosted input so compile-time
# evaluation cannot remove the loops; the CLI test pins both counts, so a
# change in what a step allocates shows up in either of them.
run! : Str => Str
run! = |input| {
    seed = Str.count_utf8_bytes(input)
    small = List.repeat(3.U64, seed * 4)
    large = List.repeat(3.U64, seed * 64)

    small_counts = chains!(small, seed)
    large_counts = chains!(large, seed)

    "sums: ${small_counts.sum.to_str()} ${large_counts.sum.to_str()}, allocations: ${small_counts.allocs.to_str()} ${large_counts.allocs.to_str()}"
}

Counts : { sum : U64, allocs : U64 }

chains! : List(U64), U64 => Counts
chains! = |xs, seed| {
    before = Host.alloc_count!()

    var $mapped = 0
    for x in xs.iter().map(|n| n * 2).keep_if(|n| n > 1) {
        $mapped = $mapped + x
    }

    var $dropped = 0
    for x in xs.iter().drop_if(|n| n == 0).map(|n| n + seed) {
        $dropped = $dropped + x
    }

    var $windowed = 0
    for x in xs.iter().drop_first(seed).take_first(seed * 2) {
        $windowed = $windowed + x
    }

    var $reversed = 0
    for x in xs.iter_rev().map(|n| n + 1) {
        $reversed = $reversed + x
    }

    var $bytes = 0
    for byte in "abc".iter_utf8() {
        $bytes = $bytes + byte.to_u64()
    }

    allocs = Host.alloc_count!() - before
    { sum: $mapped + $dropped + $windowed + $reversed + $bytes, allocs }
}
