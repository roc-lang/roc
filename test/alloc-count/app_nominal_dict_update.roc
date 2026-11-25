app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Issue 13377: both dictionaries have capacity for all n missing keys.
# The builder owns the bag, so neither update needs a fresh allocation.
Bag :: { rows : Dict(U64, Str), seen : Dict(U64, Bool) }.{
        add : Bag, U64, Str -> Bag
        add = |bag, key, row| {
                { rows, seen } = bag
                next_seen = seen.update(key, |found| match found {
                        Ok(first) => Ok(first)
                        Err(Missing) => Ok(Bool.True)
                })
                { rows: rows.insert(key, row), seen: next_seen }
        }
}

generated : Bag, U64 -> Bag
generated = |bag, n| {
        var $b = bag
        for id in 0..<n {
                $b = $b.add(id, "row")
        }
        $b
}

run! : Str => Str
run! = |input| {
        n = 64 + input.count_utf8_bytes()
        a = Bag.{ rows: Dict.with_capacity(n), seen: Dict.with_capacity(n) }
        b = Bag.{ rows: Dict.with_capacity(n + 1), seen: Dict.with_capacity(n + 1) }
        start = Host.alloc_count!()
        first = generated(a, n)
        second = generated(b, n + 1)
        count = Host.alloc_count!() - start
        ok = first.rows.len() == n and second.seen.len() == n + 1
        "lengths correct: ${Str.inspect(ok)}; allocations: ${count.to_str()}"
}
