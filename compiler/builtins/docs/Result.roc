interface Result
    exposes
        [
            Result,
            after,
            isOk,
            isErr,
            map,
            map2,
            mapErr,
            withDefault
        ]
    imports []

## The result of an operation that could fail: either the operation went
## okay, or else there was an error of some sort.
Result ok err : [ @Result ok err ]

## Return True if the result indicates a success, else return False
##
## >>> Result.isOk (Ok 5)
isOk : Result * * -> bool

## Return True if the result indicates a failure, else return False
##
## >>> Result.isErr (Err "uh oh")
isErr : Result * * -> bool

## If the result is `Ok`, return the value it holds. Otherwise, return
## the given default value.
##
## >>> Result.withDefault (Ok 7) 42
##
## >>> Result.withDefault (Err "uh oh") 42
withDefault : Result ok err, ok -> ok

## If the result is `Ok`, transform the entire result by running a conversion
## function on the value the `Ok` holds. Then return that new result.
##
## (If the result is `Err`, this has no effect. Use `afterErr` to transform an `Err`.)
##
## >>> Result.after (Ok -1) \num -> if num < 0 then Err "negative!" else Ok -num
##
## >>> Result.after (Err "yipes!") \num -> if num < 0 then Err "negative!" else Ok -num
after : Result before err, (before -> Result after err) -> Result after err

## If the result is `Ok`, transform the value it holds by running a conversion
## function on it. Then return a new `Ok` holding the transformed value.
##
## (If the result is `Err`, this has no effect. Use [mapErr] to transform an `Err`.)
##
## >>> Result.map (Ok 12) Num.negate
##
## >>> Result.map (Err "yipes!") Num.negate
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example [List.map], `Set.map`, and `Dict.map`.
map : Result before err, (before -> after) -> Result after err

## If the result is `Err`, transform the value it holds by running a conversion
## function on it. Then return a new `Err` holding the transformed value.
##
## (If the result is `Ok`, this has no effect. Use [map] to transform an `Ok`.)
##
## >>> Result.mapErr (Err "yipes!") Str.isEmpty
##
## >>> Result.mapErr (Ok 12) Str.isEmpty
mapErr : Result ok before, (before -> after) -> Result ok after

## If both of the results are `Ok`, apply a function giving both result values
## as arguments. Then return a new `Ok` holding the return value of that function.
##
## (If either result is `Err`, this has no effect.)
##
## >>> Result.map2 (Ok 2) (Ok 4) (\a, b -> a + b)
##
## None of the following will have any effect:
##
## >>> Result.map2 (Ok 2) (Err "yikes!") (\a, b -> a + b)
## 
## >>> Result.map2 (Err "yikes!") (Ok 2) (\a, b -> a + b)
##
## >>> Result.map2 (Err "double") (Err "yikes!") (\a, b -> a + b)
map2 : Result before err, Result before err, (before, before -> Result after err) -> Result after err
