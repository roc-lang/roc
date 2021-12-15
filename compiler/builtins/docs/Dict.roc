interface Dict
    exposes
        [
            Dict,
            contains,
            difference,
            empty,
            get,
            keys,
            insert,
            intersection,
            len,
            remove,
            single,
            union,
            values,
            walk
        ]
    imports []

size : Dict * * -> Nat

isEmpty : Dict * * -> Bool

## Convert each key and value in the #Dict to something new, by calling a conversion
## function on each of them. Then return a new #Map of the converted keys and values.
##
## >>> Dict.map {{ 3.14 => "pi", 1.0 => "one" }} \{ key, value } -> { key:
##
## >>> Dict.map {[ "", "a", "bc" ]} Str.isEmpty
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example [List.map], [Result.map], and `Set.map`.
map :
    Dict beforeKey beforeValue,
    ({ key: beforeKey, value: beforeValue } -> { key: afterKey, value: afterValue })
    -> Dict afterKey afterValue

# DESIGN NOTES: The reason for panicking when given NaN is that:
# * If we allowed NaN in, Dict.insert would no longer be idempotent.
# * If we allowed NaN but overrode its semantics to make it feel like "NaN == NaN" we'd need isNaN checks in all hashing operations as well as all equality checks (during collision detection), not just insert. This would be much worse for performance than panicking on insert, which only requires one extra conditional on insert.
# * It's obviously invalid; the whole point of NaN is that an error occurred. Giving a runtime error notifies you when this problem happens. Giving it only on insert is the best for performance, because it means you aren't paying for isNaN checks on lookups as well.

# TODO: removed `'` from signature because parser does not support it yet
# Original signature: insert : Dict 'key val, 'key, val -> Dict 'key val
## Make sure never to insert a key of *NaN* into a [Dict]! Because *NaN* is
## defined to be unequal to *NaN*, inserting a *NaN* key results in an entry
## that can never be retrieved or removed from the [Dict].
insert : Dict key val, key, val -> Dict key val
