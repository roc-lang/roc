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

## A [dictionary](https://en.wikipedia.org/wiki/Associative_array) that lets you can associate keys with values.
##
## ### Inserting
##
## The most basic way to use a dictionary is to start with an empty one and then:
## 1. Call [Dict.insert] passing a key and a value, to associate that key with that value in the dictionary.
## 2. Later, call [Dict.get] passing the same key as before, and it will return the value you stored.
##
## Here's an example of a dictionary which uses a city's name as the key, and its population as the associated value.
##
##     populationByCity =
##         Dict.empty
##             |> Dict.insert "London" 8_961_989
##             |> Dict.insert "Philadelphia" 1_603_797
##             |> Dict.insert "Shanghai" 24_870_895
##             |> Dict.insert "Delhi" 16_787_941
##             |> Dict.insert "Amsterdam" 872_680
##
## ### Converting to a [List]
##
## We can call [Dict.toList] on `populationByCity` to turn it into a list of key-value pairs:
##
##     Dict.toList populationByCity == [
##         { k: "London", v: 8961989 },
##         { k: "Philadelphia", v: 1603797 },
##         { k: "Shanghai", v: 24870895 },
##         { k: "Delhi", v: 16787941 },
##         { k: "Amsterdam", v: 872680 },
##     ]
##
## We can use the similar [Dict.keyList] and [Dict.values] functions to get only the keys or only the values,
## instead of getting these `{ k, v }` records that contain both.
##
## You may notice that these lists have the same order as the original insertion order. This will be true if
## all you ever do is [insert] and [get] operations on the dictionary, but [remove] operations can change this order.
## Let's see how that looks.
##
## ### Removing
##
## We can remove an element from the dictionary, like so:
##
##     populationByCity
##         |> Dict.remove "Philadelphia"
##         |> Dict.toList
##         ==
##         [
##             { k: "London", v: 8961989 },
##             { k: "Amsterdam", v: 872680 },
##             { k: "Shanghai", v: 24870895 },
##             { k: "Delhi", v: 16787941 },
##         ]
##
## Notice that the order changed! Philadelphia has been not only removed from the list, but Amsterdam - the last
## entry we inserted - has been moved into the spot where Philadelphia was previously. This is exactly what
## [Dict.remove] does: it removes an element and moves the most recent insertion into the vacated spot.
##
## This move is done as a performance optimization, and it lets [remove] have
## [constant time complexity](https://en.wikipedia.org/wiki/Time_complexity#Constant_time). If you need a removal
## operation which preserves ordering, [Dict.removeShift] will remove the element and then shift everything after it
## over one spot. Be aware that this shifting requires copying every single entry after the removed element, though,
## so it can be massively more costly than [remove]! This makes [remove] the recommended default choice;
## [removeShift] should only be used if maintaining original insertion order is absolutely necessary.
##
##
## ### Removing
##
## ### Equality
##
## When comparing two dictionaries for equality, they are `==` only if their both their contents and their
## orderings match. This preserves the property that if `dict1 == dict2`, you should be able to rely on
## `fn dict1 == fn dict2` also being `True`, even if `fn` relies on the dictionary's ordering (for example, if
## `fn` is `Dict.toList` or calls it internally.)
##
## The [Dict.hasSameContents] function gives an alternative to `==` which ignores ordering
## and returns `True` if both dictionaries have the same keys and associated values.
Dict k v : [ @Dict k v ] # TODO k should require a hashing and equating constraint

## An empty dictionary.
empty : Dict * *

size : Dict * * -> Nat

isEmpty : Dict * * -> Bool

## Returns a [List] of the dictionary's key/value pairs.
##
## See [walk] to walk over the key/value pairs without creating an intermediate data structure.
toList : Dict k v -> List { k, v }

## Returns a [List] of the dictionary's keys.
##
## See [keySet] to get a [Set] of keys instead, or [walkKeys] to walk over the keys without creating
## an intermediate data structure.
keyList : Dict key * -> List key

## Returns a [Set] of the dictionary's keys.
##
## See [keyList] to get a [List] of keys instead, or [walkKeys] to walk over the keys without creating
## an intermediate data structure.
keySet : Dict key * -> Set key

## Returns a [List] of the dictionary's values.
##
## See [walkValues] to walk over the values without creating an intermediate data structure.
values : Dict * value -> List value

walk : Dict k v, state, (state, k, v -> state) -> state

walkKeys : Dict key *, state, (state, key -> state) -> state

walkValues : Dict * value, state, (state, value -> state) -> state

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
    Dict beforeKey beforeVal,
    ({ k: beforeKey, v: beforeVal } -> { k: afterKey, v: afterVal })
    -> Dict afterKey afterVal

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

## Removes a key from the dictionary in [constant time](https://en.wikipedia.org/wiki/Time_complexity#Constant_time), without preserving insertion order.
##
## Since the internal [List] which determines the order of operations like [toList] and [walk] cannot have gaps in it,
## whenever an element is removed from the middle of that list, something must be done to eliminate the resulting gap.
##
## * [removeShift] eliminates the gap by shifting over every element after the removed one. This takes [linear time](https://en.wikipedia.org/wiki/Time_complexity#Linear_time),
## and preserves the original ordering.
## * [remove] eliminates the gap by replacing the removed element with the one at the end of the list - that is, the most recent insertion. This takes [constant time](https://en.wikipedia.org/wiki/Time_complexity#Constant_time), but does not preserve the original ordering.
##
## For example, suppose we have a `populationByCity` with these contents:
##
##     Dict.toList populationByCity == [
##         { k: "London", v: 8961989 },
##         { k: "Philadelphia", v: 1603797 },
##         { k: "Shanghai", v: 24870895 },
##         { k: "Delhi", v: 16787941 },
##         { k: "Amsterdam", v: 872680 },
##     ]
##
## Using `Dict.remove "Philadelphia"` on this will replace the `"Philadelphia"` entry with the most recent insertion,
## which is `"Amsterdam"` in this case.
##
##     populationByCity
##         |> Dict.remove "Philadelphia"
##         |> Dict.toList
##         ==
##         [
##             { k: "London", v: 8961989 },
##             { k: "Amsterdam", v: 872680 },
##             { k: "Shanghai", v: 24870895 },
##             { k: "Delhi", v: 16787941 },
##         ]
##
## Both [remove] and [removeShift] leave the dictionary with the same contents; they only differ in ordering and in
## performance. Since ordering only affects operations like [toList] and [walk], [remove] is the better default
## choice because it has much better performance characteristics; [removeShift] should only be used when it's
## absolutely necessary for operations like [toList] and [walk] to preserve the exact original insertion order.
remove : Dict k v, k -> Dict k v

## Removes a key from the dictionary in [linear time](https://en.wikipedia.org/wiki/Time_complexity#Linear_time), while preserving insertion order.
##
## It's better to use [remove] than this by default, since [remove] has [constant time complexity](https://en.wikipedia.org/wiki/Time_complexity#Constant_time),
## which commonly leads [removeShift] to take many times as long to run as [remove] does. However, [remove] does not
## preserve insertion order, so the slower [removeShift] exists only for use cases where it's abolutely necessary for
## ordering-sensitive functions like [toList] and [walk] to preserve the exact original insertion order.
##
## See the [remove] documentation for more details about the differences between [remove] and [removeShift].
removeShift : Dict k v, k -> Dict k v

## Returns whether both dictionaries have the same keys, and the same values associated with those keys.
## This is different from `==` in that it disregards the ordering of the keys and values.
hasSameContents : Dict k v, Dict k v -> Bool
