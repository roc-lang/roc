interface AssocList
    exposes [
        empty,
        single,
        insertNew,
        insertNewLazy,
        insert,
        contains,
        len,
        keys,
        values,
        get,
        walk,
        remove,
        normalizeWith,
        insertAll,
        ]
    imports [List.{ List }]

## An Association List is a list of key-value pairs.
## As such, insertion of a new key is fast (taking 'constant-time'),
## but removal of an existing key might be rather slow (taking 'linear time').
##
## Because lists are represented in Roc as flat arrays that double their capacity when needed,
## it is much cheaper to append to a list than to prepend to it.
## As such, insertion of new associations happens at the appending ('back') side of the list.
##
## Insertion order is maintained, unless elements are removed or replaced in the meantime.
##
## Two AssocLists only compare equal (with `==`) if their internal order of associations is the same.
## If you want to compare whether two AssocLists contain the same key-values,
## call [normalize] on them first.
AssocList k v := List [Pair k v]


## Creates a new, empty AssocList
##
## Runs in constant time.
empty : AssocList k v
empty = @AssocList []

## Creates a new AssocList containing the single association `k` mapping to `v`
##
## Runs in constant time.
single : k, v -> AssocList k v
single = \k, v ->
    @AssocList [Pair k v]

## Internal helper function to insert a new association
##
## Precondition: `k` should not exist in the AssocList yet.
insertFresh : AssocList k v, k, v -> AssocList k v
insertFresh = \@AssocList list, k, v ->
    list
      |> List.append (Pair k v)
      |> @AssocList

## Returns true iff a value is currently associated with the given key.
contains : AssocList k v, k -> Bool
contains = \@AssocList list, needle ->
    list
        |> List.find (\Pair key _val -> key == needle)
        |> Result.isOk

## Inserts a new association, but only if the key was not yet in the association list.
## Otherwise, returns the list unchanged.
##
## Runs in linear time (because we need to check for duplicates).
insertNew : AssocList k v, k, v -> AssocList k v
insertNew = \assocList, k, v ->
    insertNewLazy assocList k (\{} -> v)

## Inserts a new association, but only if the key was not yet in the association list.
## Otherwise, returns the list unchanged.
##
## Variant of [insertNew] that only builds the new value if it will be used.
##
## Runs in linear time (because we need to check for duplicates).
insertNewLazy : AssocList k v, k, ({} -> v) -> AssocList k v
insertNewLazy = \assocList, k, fun ->
    if contains assocList k then
        assocList
    else
        insertFresh assocList k (fun {})

## Inserts a new association, overriding an existing one if an association of k already existed.
##
## Runs in linear time (because we need to check for duplicates).
insert : AssocList k v, k, v -> AssocList k v
insert = \@AssocList list, k, v ->

    when listFindIndex list (\Pair key _ -> key == k) is
        Err NotFound ->
            insertFresh (@AssocList list) k v
        Ok index ->
            list
                |> List.set index (Pair k v)
                |> @AssocList

# NOTE: This helper function might be moved into the List module someday:
listFindIndex : List elem, (elem -> Bool) -> Result Nat [NotFound]*
listFindIndex = \list, matcher ->
    foundIndex = List.walkUntil list 0 (\index, elem -> if matcher elem then Stop index else Continue (index + 1))

    if foundIndex < List.len list then
        Ok foundIndex
    else
        Err NotFound

## Returns the number of associations in the AssocList
len : AssocList k v -> Nat
len = \@AssocList list ->
    List.len list

## Returns a [List] of the AssocList's keys
keys : AssocList k v -> List k
keys = \@AssocList list ->
    List.map list (\Pair k _ -> k)

## Returns a [List] of the AssocList's values
values : AssocList k v -> List v
values = \@AssocList list ->
    List.map list (\Pair _ v -> v)

get : AssocList k v, k -> Result v [KeyNotFound]*
get = \@AssocList list, needle ->
    list
        |> List.find (\Pair key _ -> key == needle)
        |> Result.map (\Pair _ v -> v)
        |> Result.mapErr (\NotFound -> KeyNotFound)

walk : AssocList k v, state, (state, k, v -> state) -> state
walk = \@AssocList list, initialState, transform ->
    List.walk list initialState (\state, Pair k v -> transform state k v)

## Removes the given key from the AssocList
## (Returns the AssocList unchanged if it was not inside)
##
## Upon removal, the most recently inserted key-value pair
## is moved into the place of the removed pair, for increased memory efficiency.
## This means that after calling remove, insertion order is not maintained.
remove : AssocList k v, k -> AssocList k v
remove = \@AssocList list, key ->
    when listFindIndex list (\Pair k _ -> k == key) is
        Err NotFound ->
            @AssocList list
        Ok index ->
            lastIndex = List.len list - 1

            list
                |> List.swap index lastIndex
                |> List.dropLast
                |> @AssocList

## Normalizes an AssocList, storing all keys internally in the same order.
##
## As mentioned in the module documentation, an AssocList keeps track of insertion order
## (as long as only insertions happen).
## This does mean that two AssocLists might not compare equal (with ==),
## even if they contain the same key-value associations.
##
## To make sure that two AssocLists containing the same keys compare equal,
## call normalizeWith (with the same sorter function)
## on both of them before comparison.
normalizeWith : AssocList k v, (k, k -> [LT, EQ, GT])  -> AssocList k v
normalizeWith = \@AssocList list, sorter ->
    list
      |> List.sortWith (\Pair k1 _v1, Pair k2 _v2 -> sorter k1 k2)
      |> @AssocList

## Returns an association list where all associations of the second parameter
## have been inserted into the first parameter.
## This means that on conflict, the association inside the second parameter is kept.
##
## Note that this uses plain insertion internally, and as such is rather inefficient:
## It runs in quadratic time.
## (`O(n * m)` where `n` and `m` are the number of elements in the first and second parameters, respectively.
insertAll : AssocList k v, AssocList k v -> AssocList k v
insertAll = \xs, ys ->
          walk ys xs \result, key, value ->
              insert result key value
