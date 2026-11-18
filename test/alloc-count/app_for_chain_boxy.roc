app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Issue 12854's adapter chains under `--specialize=no`: a `for` loop over a
# chain of iterator adapters steps each adapter's step closure, and an
# adapter's step builds the next one around its upstream's, so these loops
# allocate per step. Each chain runs over a small and a large input; the
# difference in allocations, divided by the difference in elements pulled,
# is what one element costs. Every chain must cost at most one allocation per
# element for each stepping stage it has (the source and each adapter), and
# the CLI test pins each chain's per-element cost, so a step that builds more
# closures shows up as a larger cost. Every length depends on the hosted
# input so compile-time evaluation cannot remove the loops.
run! : Str => Str
run! = |input| {
    seed = Str.count_utf8_bytes(input)
    small = List.repeat(3.U64, seed * 4)
    large = List.repeat(3.U64, seed * 64)
    small_text = input
    large_text = Str.join_with(List.repeat(input, 16), "")

    mapped = per_element(mapped!(small), mapped!(large), 3)
    dropped = per_element(dropped!(small, seed), dropped!(large, seed), 3)
    windowed = per_element(windowed!(small, seed), windowed!(large, seed), 3)
    reversed = per_element(reversed!(small), reversed!(large), 2)
    bytes = per_element(bytes!(small_text), bytes!(large_text), 1)

    costs = [mapped, dropped, windowed, reversed, bytes]
    within = List.all(costs, |cost| cost.within)
    sum = List.fold(costs, 0, |acc, cost| acc + cost.sum)
    texts = Str.join_with(List.map(costs, |cost| cost.text), " ")

    "sum: ${sum.to_str()}, allocations per element: ${texts}, within one per stage: ${if within "yes" else "no"}"
}

Run : { sum : U64, allocs : U64, items : U64 }

Cost : { sum : U64, text : Str, within : Bool }

# The allocations one more element costs, as a quotient and remainder of the
# difference between the two runs.
per_element : Run, Run, U64 -> Cost
per_element = |small, large, stages| {
    allocs = large.allocs - small.allocs
    items = large.items - small.items
    {
        sum: small.sum + large.sum,
        text: "${(allocs // items).to_str()}r${(allocs % items).to_str()}",
        within: allocs <= stages * items,
    }
}

mapped! : List(U64) => Run
mapped! = |xs| {
    before = Host.alloc_count!()
    var $sum = 0
    var $items = 0
    for x in xs.iter().map(|n| n * 2).keep_if(|n| n > 1) {
        $sum = $sum + x
        $items = $items + 1
    }
    { sum: $sum, allocs: Host.alloc_count!() - before, items: $items }
}

dropped! : List(U64), U64 => Run
dropped! = |xs, seed| {
    before = Host.alloc_count!()
    var $sum = 0
    var $items = 0
    for x in xs.iter().drop_if(|n| n == 0).map(|n| n + seed) {
        $sum = $sum + x
        $items = $items + 1
    }
    { sum: $sum, allocs: Host.alloc_count!() - before, items: $items }
}

windowed! : List(U64), U64 => Run
windowed! = |xs, seed| {
    before = Host.alloc_count!()
    var $sum = 0
    var $items = 0
    for x in xs.iter().drop_first(seed).take_first(xs.len() // 2) {
        $sum = $sum + x
        $items = $items + 1
    }
    { sum: $sum, allocs: Host.alloc_count!() - before, items: $items }
}

reversed! : List(U64) => Run
reversed! = |xs| {
    before = Host.alloc_count!()
    var $sum = 0
    var $items = 0
    for x in xs.iter_rev().map(|n| n + 1) {
        $sum = $sum + x
        $items = $items + 1
    }
    { sum: $sum, allocs: Host.alloc_count!() - before, items: $items }
}

bytes! : Str => Run
bytes! = |text| {
    before = Host.alloc_count!()
    var $sum = 0
    var $items = 0
    for byte in text.iter_utf8() {
        $sum = $sum + byte.to_u64()
        $items = $items + 1
    }
    { sum: $sum, allocs: Host.alloc_count!() - before, items: $items }
}
