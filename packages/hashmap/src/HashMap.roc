interface HashMap
    exposes
        [
            HashMap,

            contains,
            difference,
            empty,
            get,
            insert,
            intersection,
            len,
            remove,
            single,
            union,

            fromList,
            isEmpty,
            filter,
            map,
            update,
        ]
    imports []

## There are a number of functions that you might expect in a Hashmap
## implementation that are missing. There is a reason for this: We want our
## hash function to be an implementation detail THAT WE CAN CHANGE!!!!!
## These functions leak out details of the hash function; In particular
## the order of the list returned by toList is notorious for being a headache
## to package maintainers.
## [Citations needed.]
##
## So, toList is not provided. The functions keys, and values have the same
## issue. What might not be so obvious is that walk is also an issue because
## it is possible to implement toList using walk:
## toList = \m -> walk (\k, v, l -> List.append l {k, v}) []


HashMap k v : [ Empty, Leaf k v ]

empty : HashMap k v
empty = Empty

todo : HashMap k v
todo = Empty

single : k, v -> HashMap k v
single = \_, _ -> todo

insert : HashMap k v, k, v -> HashMap k v
insert = \_, _, _ -> todo

update: HashMap k v, k, (v -> v) -> HashMap k v
update = \_, _, _ -> todo

remove : HashMap k v, k -> HashMap k v
remove = \_, _ -> todo

isEmpty: HashMap k v -> Bool
isEmpty = \m -> 
    when m is
        Empty -> True
        _ -> False

get : HashMap k v, k -> Result v {}
get = \m, _ -> 
    when m is
        Empty -> Err {}
        Leaf _ v -> Ok v

contains: HashMap k v, k -> Bool
contains = \m, k ->
    when get m k is
        Ok _ ->   True
        Err {} -> False

len : HashMap k v -> Int*
len = \m ->
    when m is
        Empty -> 0
        Leaf _ _ -> 1

fromList : List {k, v} -> HashMap k v
fromList = \_ -> todo

map : HashMap k v, (k, v -> w) -> HashMap k w
map = \_, _ -> todo

filter : HashMap k v, (k, v -> Bool) -> HashMap k v
filter = \_, _ -> todo

union : HashMap k v, HashMap k v -> HashMap k v
union = \_, _ -> todo

intersection : HashMap k v, HashMap k w -> HashMap k v
intersection = \_, _ -> todo

difference : HashMap k v, HashMap k w -> HashMap k v
difference = \_, _ -> todo



