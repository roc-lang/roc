app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# An unannotated function is generic in the fields of the record it takes
# apart. Rebuilding that record around an appended list must move the list
# out of the record it replaces, so appends stay in place. This pins
# allocations, not noisy elapsed time.

push = |{ tag, items }, x| { tag, items: items.append(x) }

grow : U64 -> U64
grow = |n| {
    var $record = { tag: "plans", items: [] }
    var $index = 0.U64
    while $index < n {
        $record = push($record, "Hello")
        $index = $index + 1
    }
    $record.items.len()
}

run! : Str => Str
run! = |input| {
    n = Str.count_utf8_bytes(input).to_u64() * 64
    before = Host.alloc_count!()
    len = grow(n)
    allocations = Host.alloc_count!() - before
    "correct: ${Str.inspect(len == n)}, amortized: ${Str.inspect(allocations <= 64)}, allocations: ${allocations.to_str()}"
}
