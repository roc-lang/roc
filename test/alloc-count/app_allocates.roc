app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Positive control for the allocation-counting harness. Str.to_utf8 copies the
# runtime input, so this region must make the counter increase.
run! : Str => Str
run! = |input| {
    before = Host.alloc_count!()
    bytes = Str.to_utf8(input)
    allocation_count = Host.alloc_count!() - before

    expect allocation_count > 0

    "byte count: ${List.len(bytes).to_str()}, allocations: ${allocation_count.to_str()}"
}
