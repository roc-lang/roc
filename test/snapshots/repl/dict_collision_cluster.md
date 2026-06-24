# META
~~~ini
description=Dict handles many keys with identical hashes through insert, overwrite, remove, and reinsert
type=repl
~~~
# SOURCE
~~~roc
» HashClash := [Id(U64)].{
    is_eq : HashClash, HashClash -> Bool
    is_eq = |a, b| match a {
        Id(a_id) => match b {
            Id(b_id) => a_id == b_id
        }
    }

    to_hash : HashClash, Hasher -> Hasher
    to_hash = |_key, hasher| Hasher.write_u64(hasher, 0)
}
» clash = |n| HashClash.Id(n)
» dict = Dict.empty().insert(clash(0), "zero").insert(clash(1), "one").insert(clash(2), "two").insert(clash(3), "three").insert(clash(4), "four").insert(clash(5), "five").insert(clash(6), "six").insert(clash(7), "seven").insert(clash(8), "eight").insert(clash(9), "nine").insert(clash(10), "ten").insert(clash(11), "eleven")
» dict.len()
» [dict.get(clash(0)), dict.get(clash(5)), dict.get(clash(11)), dict.get(clash(99))]
» overwritten = dict.insert(clash(5), "FIVE")
» overwritten.len()
» [overwritten.get(clash(5)), overwritten.get(clash(6))]
» removed = overwritten.remove(clash(0)).remove(clash(5)).remove(clash(11))
» removed.len()
» [removed.get(clash(1)), removed.get(clash(6)), removed.get(clash(10)), removed.get(clash(0)), removed.get(clash(5)), removed.get(clash(11))]
» reinserted = removed.insert(clash(5), "five again").insert(clash(12), "twelve")
» [reinserted.get(clash(5)), reinserted.get(clash(12)), reinserted.get(clash(10))]
~~~
# OUTPUT
assigned `HashClash`
---
assigned `clash`
---
assigned `dict`
---
12
---
[Ok("zero"), Ok("five"), Ok("eleven"), Err(KeyNotFound)]
---
assigned `overwritten`
---
12
---
[Ok("FIVE"), Ok("six")]
---
assigned `removed`
---
9
---
[Ok("one"), Ok("six"), Ok("ten"), Err(KeyNotFound), Err(KeyNotFound), Err(KeyNotFound)]
---
assigned `reinserted`
---
[Ok("five again"), Ok("twelve"), Ok("ten")]
# PROBLEMS
NIL
