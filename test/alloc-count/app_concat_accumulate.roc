app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Accumulating with `List.concat` or `List.append` onto a uniquely owned list
# must grow its capacity geometrically, so N operations perform O(log N)
# growth reallocations instead of one per call.
run! : Str => Str
run! = |input| {
    n = Str.count_utf8_bytes(input).to_u64() * 64
    pair = [n, n + 1]

    concat_before = Host.alloc_count!()
    var $concatenated = []
    var $i = 0
    while $i < n {
        $concatenated = $concatenated.concat(pair)
        $i = $i + 1
    }
    concat_allocs = Host.alloc_count!() - concat_before

    append_before = Host.alloc_count!()
    var $appended = []
    var $j = 0
    while $j < n {
        $appended = $appended.append(n).append(n + 1)
        $j = $j + 1
    }
    append_allocs = Host.alloc_count!() - append_before

    concat_amortized = concat_allocs <= 32
    append_amortized = append_allocs <= 32
    expect concat_amortized
    expect append_amortized

    "items: ${List.len($concatenated).to_str()} ${List.len($appended).to_str()}, concat amortized: ${Str.inspect(concat_amortized)}, append amortized: ${Str.inspect(append_amortized)}"
}
