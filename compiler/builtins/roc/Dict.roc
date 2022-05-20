interface Dict
    exposes
        [
            empty,
            single,
            get,
            walk,
            insert,
            len,
            remove,
            contains,
            keys,
            values,
            union,
            intersection,
            difference,
        ]
    imports
        [
            Bool.{ Bool },
        ]

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
## ### Accessing keys or values
##
## We can use [Dict.keys] and [Dict.values] functions to get only the keys or only the values.
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
##         |> Dict.keys
##         ==
##         [ "London", "Amsterdam", "Shanghai", "Delhi" ]
##
## Notice that the order changed! Philadelphia has been not only removed from the list, but Amsterdam - the last
## entry we inserted - has been moved into the spot where Philadelphia was previously. This is exactly what
## [Dict.remove] does: it removes an element and moves the most recent insertion into the vacated spot.
##
## This move is done as a performance optimization, and it lets [remove] have
## [constant time complexity](https://en.wikipedia.org/wiki/Time_complexity#Constant_time). ##
##
## ### Equality
##
## When comparing two dictionaries for equality, they are `==` only if their both their contents and their
## orderings match. This preserves the property that if `dict1 == dict2`, you should be able to rely on
## `fn dict1 == fn dict2` also being `True`, even if `fn` relies on the dictionary's ordering.
## An empty dictionary.
empty : Dict k v
single : k, v -> Dict k v
get : Dict k v, k -> Result v [KeyNotFound]*
walk : Dict k v, state, (state, k, v -> state) -> state
insert : Dict k v, k, v -> Dict k v
len : Dict k v -> Nat
remove : Dict k v, k -> Dict k v
contains : Dict k v, k -> Bool

## Returns a [List] of the dictionary's keys.
keys : Dict k v -> List k

## Returns a [List] of the dictionary's values.
values : Dict k v -> List v
union : Dict k v, Dict k v -> Dict k v
intersection : Dict k v, Dict k v -> Dict k v
difference : Dict k v, Dict k v -> Dict k v
