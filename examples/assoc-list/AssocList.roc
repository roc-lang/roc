interface AssocList
    exposes []
    imports [List.{ List }]

AssocList k v : (List [Pair k v])

#  ## An empty dictionary.
#  empty : Dict k v
#  single : k, v -> Dict k v
#  get : Dict k v, k -> Result v [KeyNotFound]*
#  walk : Dict k v, state, (state, k, v -> state) -> state
#  insert : Dict k v, k, v -> Dict k v
#  len : Dict k v -> Nat
#  remove : Dict k v, k -> Dict k v
#  contains : Dict k v, k -> Bool

#  ## Returns a [List] of the dictionary's keys.
#  keys : Dict k v -> List k

#  ## Returns a [List] of the dictionary's values.
#  values : Dict k v -> List v
#  union : Dict k v, Dict k v -> Dict k v
#  intersection : Dict k v, Dict k v -> Dict k v
#  difference : Dict k v, Dict k v -> Dict k v

## Creates a new, empty AssocList
empty : AssocList k v
empty = []

## Creates a new AssocList containing the single association `k` mapping to `v`
single : k, v -> AssocList k v
single = \k, v ->
    [Pair k v]

## Returns true iff a value is currently associated with the given key.
contains : AssocList k v, k -> Bool
contains = \list, needle ->
    list
    |> List.find (\Pair key _val -> key == needle)
    |> Result.isOk

## Returns the number of associations in the AssocList
len : AssocList k v -> Nat
len = \list ->
    List.len list

## Returns a [List] of the AssocList's keys
keys : AssocList k v -> List k
keys = \list ->
    List.map list (\Pair k _ -> k)


## Returns a [List] of the AssocList's values
values : AssocList k v -> List v
values = \list ->
    List.map list (\Pair _ v -> v)
