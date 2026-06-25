# META
~~~ini
description=Dict.from_list keeps the last value for duplicate colliding keys
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
» dict = Dict.from_list([(clash(1), "one"), (clash(2), "two"), (clash(1), "ONE"), (clash(3), "three"), (clash(2), "TWO"), (clash(4), "four")])
» dict.len()
» [dict.get(clash(1)), dict.get(clash(2)), dict.get(clash(3)), dict.get(clash(4)), dict.get(clash(5))]
~~~
# OUTPUT
assigned `HashClash`
---
assigned `clash`
---
assigned `dict`
---
4
---
[Ok("ONE"), Ok("TWO"), Ok("three"), Ok("four"), Err(KeyNotFound)]
# PROBLEMS
NIL
