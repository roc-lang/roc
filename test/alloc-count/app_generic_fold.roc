app [run!] { pf: platform "./platform/main.roc" }

import pf.Host
import GenericFold

run! : Str => Str
run! = |input| {
    n = 256.U64 + input.count_utf8_bytes().to_u64()
    item = input.count_utf8_bytes().to_u64()
    items = List.repeat(item, n)
    before = Host.alloc_count!()
    result = GenericFold.append_all([], items)
    allocations = Host.alloc_count!() - before
    correct = result.len() == n and result == items
    amortized = allocations <= 64

    original = [input.concat(" kept across a generic callback")]
    extra = input.concat(" appended through a generic callback")
    changed = GenericFold.append_all(original, [extra])
    alias_safe = original == [input.concat(" kept across a generic callback")] and changed == original.append(extra)
    unused_safe = GenericFold.discard_field(original, extra, [input.concat(" unused owned field")]) == changed
    (left, right) = GenericFold.duplicate_fields(original, extra)
    duplicated_safe = left == changed and right == original
    deferred_safe = GenericFold.append_deferred(original, [extra, extra]) == changed.append(extra)
    "correct: ${Str.inspect(correct)}, amortized: ${Str.inspect(amortized)}, alias safe: ${Str.inspect(alias_safe)}, unused safe: ${Str.inspect(unused_safe)}, duplicated safe: ${Str.inspect(duplicated_safe)}, deferred safe: ${Str.inspect(deferred_safe)}, allocations: ${allocations.to_str()}"
}
