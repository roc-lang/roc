interface AssocList
    exposes []
    imports [List.{ List }]

## An Association List is a list of key-value pairs.
## As such, insertion of a new key is fast (taking 'constant-time'),
## but removal of an existing key might be rather slow (taking 'linear time').
##
## Because lists are represented in Roc as flat arrays that double their capacity when needed,
## it is much cheaper to append to a list than to prepend to it.
## As such, insertion happens at the appending side of the list.
##
## Note that an association list by itself does not care about duplicate keys.
## Insertion, lookup and replacement functions exist in multiple flavours,
## depending on how you want to deal with duplicate keys.
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
##
## Runs in constant time.
empty : AssocList k v
empty = []

## Creates a new AssocList containing the single association `k` mapping to `v`
##
## Runs in constant time.
single : k, v -> AssocList k v
single = \k, v ->
    [Pair k v]

## Internal helper function to insert a new association
##
## Precondition: `k` should not exist in the AssocList yet.
insertFresh : AssocList k v, k, v -> AssocList k v
insertFresh = \list, k, v ->
    List.append list (Pair k v)

## Returns true iff a value is currently associated with the given key.
contains : AssocList k v, k -> Bool
contains = \list, needle ->
    list
    |> List.find (\Pair key _val -> key == needle)
    |> Result.isOk

## Inserts a new association, but only if the key was not yet in the association list.
## Otherwise, returns the list unchanged.
##
## Runs in linear time (because we need to check for duplicates).
insertNew : AssocList k v, k, v -> AssocList k v
insertNew = \list, k, v ->
    insertNewLazy list k (\Unit -> v)

## Inserts a new association, but only if the key was not yet in the association list.
## Otherwise, returns the list unchanged.
##
## Variant of [insertNew] that only builds the new value if it will be used.
##
## Runs in linear time (because we need to check for duplicates).
insertNewLazy : AssocList k v, k, ([Unit] -> v) -> AssocList k v
insertNewLazy = \list, k, fun ->
    if contains list k then
        list
    else
        insertFresh list k (fun Unit)

## Inserts a new association, overriding an existing one if an association of k already existed.
##
## Runs in linear time (because we need to check for duplicates).
insert : AssocList k v, k, v -> AssocList k v
insert = \list, k, v ->

    when listFindIndex list (\Pair key _ -> key == k) is
        Err NotFound ->
            insertFresh list k v
        Ok index ->
            List.set list index (Pair k v)


# NOTE: This helper function might be moved into the List module someday:
listFindIndex : (List elem), (elem -> Bool) -> (Result Nat [ NotFound ]*)
listFindIndex = \list, matcher ->
    foundIndex = List.walkUntil list 0 (\index, elem -> if matcher elem then Stop index else Continue (index + 1))
    if foundIndex < List.len list then
        Ok foundIndex
    else
        Err NotFound

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

get : AssocList k v, k -> Result v [ KeyNotFound ]*
get = \list, needle ->
    list
    |> List.find (\Pair key _ -> key == needle)
    |> Result.map (\Pair _ v -> v)
    |> Result.mapErr (\NotFound -> KeyNotFound)

walk : AssocList k v, state, (state, k, v -> state) -> state
walk = \list, initialState, transform ->
    List.walk list initialState (\state, Pair k v-> transform state k v)

## Removes the given key from the AssocList
## (Returns the AssocList unchanged if it was not inside)
##
## Upon removal, the most recently inserted key-value pair
## is moved into the place of the removed pair, for increased memory efficiency.
## This means that after calling remove, insertion order is not maintained.
remove : AssocList k v, k -> AssocList k v
remove = \list, key ->
    if List.isEmpty list then
        list
    else
        when listFindIndex list (\Pair k _ -> k == key) is
            Err NotFound ->
                list
            Ok index ->
              lastIndex = (List.len list) - 1

              list
              |> List.swap index lastIndex
              |> List.dropLast
