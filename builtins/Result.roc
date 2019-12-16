interface Result
    exposes [ Result, map, mapAll ]


Result ok err : [ Ok ok, Err err ]

## If the #Result is `Ok`, call the given function to transform it, and return
## the transformed value tagged with `Ok`.
##
## >>> Result.map (Ok 5) \num -> num - 1
##
## If the #Result is `Err`, return that `Err` value.
##
## >>> Result.map (Err "whoops") \num -> num - 1
##
## To transform more than one #Result at once, see #mapAll.
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example #List.map, #Set.map, and #Map.map.
map : Result before err, (before -> after) -> Result after err


## If all the given #Result values are `Ok`, call the given function to transform
## them all into another value, and return the transformed value tagged with `Ok`.
##
## If any of the given #Result values is `Err`, returns the first of those `Err` values.
mapAll :
    Result a err, Result b err, (a, b -> c) -> Result c err |
    Result a err, Result b err, Result c err, (a, b, c -> d) -> Result d err |
    Result a err, Result b err, Result c err, Result d err, (a, b, c, d -> e) -> Result e err
