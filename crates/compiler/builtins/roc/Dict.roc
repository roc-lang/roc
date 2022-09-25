interface Dict
    exposes [
        Dict,
        empty,
        withCapacity,
        single,
        get,
        walk,
        insert,
        len,
        remove,
        contains,
        keys,
        values,
        insertAll,
        keepShared,
        removeAll,
    ]
    imports [
        Bool.{ Bool },
        Result.{ Result },
        List,
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
##         |> Dict.insert "London" 8_961_989
##         |> Dict.insert "Philadelphia" 1_603_797
##         |> Dict.insert "Shanghai" 24_870_895
##         |> Dict.insert "Delhi" 16_787_941
##         |> Dict.insert "Amsterdam" 872_680
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
##         ["London", "Amsterdam", "Shanghai", "Delhi"]
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
## `fn dict1 == fn dict2` also being `Bool.true`, even if `fn` relies on the dictionary's ordering.
Dict k v := List [Pair k v]

## An empty dictionary.
empty : Dict k v
empty = @Dict []

withCapacity : Nat -> Dict k v
withCapacity = \n -> @Dict (List.withCapacity n)

get : Dict k v, k -> Result v [KeyNotFound]*
get = \@Dict list, needle ->
    when List.findFirst list (\Pair key _ -> key == needle) is
        Ok (Pair _ v) ->
            Ok v

        Err NotFound ->
            Err KeyNotFound

walk : Dict k v, state, (state, k, v -> state) -> state
walk = \@Dict list, initialState, transform ->
    List.walk list initialState (\state, Pair k v -> transform state k v)

insert : Dict k v, k, v -> Dict k v
insert = \@Dict list, k, v ->
    when List.findFirstIndex list (\Pair key _ -> key == k) is
        Err NotFound ->
            insertFresh (@Dict list) k v

        Ok index ->
            list
            |> List.set index (Pair k v)
            |> @Dict

len : Dict k v -> Nat
len = \@Dict list ->
    List.len list

remove : Dict k v, k -> Dict k v
remove = \@Dict list, key ->
    when List.findFirstIndex list (\Pair k _ -> k == key) is
        Err NotFound ->
            @Dict list

        Ok index ->
            lastIndex = List.len list - 1

            list
            |> List.swap index lastIndex
            |> List.dropLast
            |> @Dict

contains : Dict k v, k -> Bool
contains = \@Dict list, needle ->
    step = \_, Pair key _val ->
        if key == needle then
            Break {}
        else
            Continue {}

    when List.iterate list {} step is
        Continue _ -> Bool.false
        Break _ -> Bool.true

single : k, v -> Dict k v
single = \key, value ->
    @Dict [Pair key value]

## Returns a [List] of the dictionary's keys.
keys : Dict k v -> List k
keys = \@Dict list ->
    List.map list (\Pair k _ -> k)

## Returns a [List] of the Dict's values
values : Dict k v -> List v
values = \@Dict list ->
    List.map list (\Pair _ v -> v)

# union : Dict k v, Dict k v -> Dict k v
insertAll : Dict k v, Dict k v -> Dict k v
insertAll = \xs, @Dict ys ->
    List.walk ys xs (\state, Pair k v -> Dict.insertIfVacant state k v)

# intersection : Dict k v, Dict k v -> Dict k v
keepShared : Dict k v, Dict k v -> Dict k v
keepShared = \@Dict xs, ys ->
    List.keepIf xs (\Pair k _ -> Dict.contains ys k)
    |> @Dict

# difference : Dict k v, Dict k v -> Dict k v
removeAll : Dict k v, Dict k v -> Dict k v
removeAll = \xs, @Dict ys ->
    List.walk ys xs (\state, Pair k _ -> Dict.remove state k)

## Internal helper function to insert a new association
##
## Precondition: `k` should not exist in the Dict yet.
insertFresh : Dict k v, k, v -> Dict k v
insertFresh = \@Dict list, k, v ->
    list
    |> List.append (Pair k v)
    |> @Dict

insertIfVacant : Dict k v, k, v -> Dict k v
insertIfVacant = \dict, key, value ->
    if Dict.contains dict key then
        dict
    else
        Dict.insert dict key value
