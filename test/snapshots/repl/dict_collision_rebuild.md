# META
~~~ini
description=Dict rebuilds bucket metadata correctly for colliding keys after keep_if, map, and release_excess_capacity
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
» dict = Dict.empty().insert(clash(0), "zero").insert(clash(1), "one").insert(clash(2), "two").insert(clash(3), "three").insert(clash(4), "four").insert(clash(5), "five").insert(clash(6), "six").insert(clash(7), "seven").insert(clash(8), "eight")
» kept = dict.keep_if(|(key, _value)| match key {
    Id(id) => id == 0 or id == 2 or id == 4 or id == 6 or id == 8
})
» kept.len()
» [kept.get(clash(0)), kept.get(clash(2)), kept.get(clash(8)), kept.get(clash(1)), kept.get(clash(7))]
» mapped = kept.map(|_key, value| Str.concat(value, "!"))
» [mapped.get(clash(0)), mapped.get(clash(6)), mapped.get(clash(8))]
» compacted = mapped.release_excess_capacity()
» [compacted.get(clash(0)), compacted.get(clash(4)), compacted.get(clash(8)), compacted.get(clash(3))]
~~~
# OUTPUT
assigned `HashClash`
---
assigned `clash`
---
assigned `dict`
---
assigned `kept`
---
5
---
[Ok("zero"), Ok("two"), Ok("eight"), Err(KeyNotFound), Err(KeyNotFound)]
---
assigned `mapped`
---
[Ok("zero!"), Ok("six!"), Ok("eight!")]
---
assigned `compacted`
---
[Ok("zero!"), Ok("four!"), Ok("eight!"), Err(KeyNotFound)]
# PROBLEMS
NIL
