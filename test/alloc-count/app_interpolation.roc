app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# An interpolated `Str` literal writes its parts once: a small result
# allocates nothing and a heap result allocates once. A custom type's assembler
# receives its values as an iterator built on the stack, so a literal of any
# width allocates only what the assembler itself does.
Count := [Count(U64)].{
    from_interpolation : List(Str) -> Try((Iter(item) -> Count), [InvalidInterpolation(Str)])
    from_interpolation = |_segments| Ok(|values| Count.Count(values.fold(0, |count, _| count + 1)))
}

run! : Str => Str
run! = |input| {
    n = Str.count_utf8_bytes(input).to_u64()
    digits = n.to_str()
    big = Str.repeat(input, 16)

    small_before = Host.alloc_count!()
    small = "${digits}-${digits}-${digits}"
    small_allocs = Host.alloc_count!() - small_before

    heap_before = Host.alloc_count!()
    heap = "${big}|${digits}|${big}"
    heap_allocs = Host.alloc_count!() - heap_before

    count_before = Host.alloc_count!()
    counted : Count
    counted = "${n}${n}${n}${n}${n}${n}${n}${n}"
    count_allocs = Host.alloc_count!() - count_before

    "interpolation allocations: ${small_allocs.to_str()} ${heap_allocs.to_str()} ${count_allocs.to_str()} (${small} ${Str.count_utf8_bytes(heap).to_str()} ${Str.inspect(counted)})"
}
