app [run!] { pf: platform "./platform/main.roc" }

import pf.Host
import GenericProductBuilder

# A runtime-sized generic builder crossing a module boundary must retain
# geometric list growth. This pins allocations, not noisy elapsed time.

grow : U64 -> Try(U64, [Full])
grow = |n| {
    var $builder = { items: [], depths: [] }
    var $index = 0.U64
    while $index < n {
        first = GenericProductBuilder.push($builder, $index)?
        second = GenericProductBuilder.push(first.builder, first.id)?
        $builder = second.builder
        $index = $index + 1
    }
    Ok($builder.items.len())
}

run! : Str => Str
run! = |input| {
    n = Str.count_utf8_bytes(input).to_u64() * 64
    before = Host.alloc_count!()
    result = grow(n)
    allocations = Host.alloc_count!() - before
    correct = result == Ok(n * 2)
    amortized = allocations <= 64

    original = { items: [input.concat(" retained across an imported generic call")], depths: [7.U64] }
    alias_safe = match GenericProductBuilder.push(original, "another long string appended to its copy") {
        Ok(next) => original.items == [input.concat(" retained across an imported generic call")] and next.builder.items.len() == 2 and next.builder.depths == [7, 1]
        Err(_) => False
    }
    "correct: ${Str.inspect(correct)}, amortized: ${Str.inspect(amortized)}, alias safe: ${Str.inspect(alias_safe)}, allocations: ${allocations.to_str()}"
}
