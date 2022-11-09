interface Result
    exposes [Result, isOk, isErr, map, mapErr, try, onErr, withDefault]
    imports [Bool.{ Bool }]

## The result of an operation that could fail: either the operation went
## okay, or else there was an error of some sort.
Result ok err : [Ok ok, Err err]

## Return `Bool.true` if the result indicates a success, else return `Bool.false`
##
## >>> Result.isOk (Ok 5)
isOk : Result ok err -> Bool
isOk = \result ->
    when result is
        Ok _ -> Bool.true
        Err _ -> Bool.false

## Return `Bool.true` if the result indicates a failure, else return `Bool.false`
##
## >>> Result.isErr (Err "uh oh")
isErr : Result ok err -> Bool
isErr = \result ->
    when result is
        Ok _ -> Bool.false
        Err _ -> Bool.true

## If the result is `Ok`, return the value it holds. Otherwise, return
## the given default value.
##
## >>> Result.withDefault (Ok 7) 42
##
## >>> Result.withDefault (Err "uh oh") 42
withDefault : Result ok err, ok -> ok
withDefault = \result, default ->
    when result is
        Ok value -> value
        Err _ -> default

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
map : Result a err, (a -> b) -> Result b err
map = \result, transform ->
    when result is
        Ok v -> Ok (transform v)
        Err e -> Err e

## If the result is `Err`, transform the value it holds by running a conversion
## function on it. Then return a new `Err` holding the transformed value.
##
## (If the result is `Ok`, this has no effect. Use [map] to transform an `Ok`.)
##
## >>> Result.mapErr (Err "yipes!") Str.isEmpty
##
## >>> Result.mapErr (Ok 12) Str.isEmpty
mapErr : Result ok a, (a -> b) -> Result ok b
mapErr = \result, transform ->
    when result is
        Ok v -> Ok v
        Err e -> Err (transform e)

## If the result is `Ok`, transform the entire result by running a conversion
## function on the value the `Ok` holds. Then return that new result.
##
## (If the result is `Err`, this has no effect. Use `onErr` to transform an `Err`.)
##
## >>> Result.try (Ok -1) \num -> if num < 0 then Err "negative!" else Ok -num
##
## >>> Result.try (Err "yipes!") \num -> if num < 0 then Err "negative!" else Ok -num
try : Result a err, (a -> Result b err) -> Result b err
try = \result, transform ->
    when result is
        Ok v -> transform v
        Err e -> Err e

## If the result is `Err`, transform the entire result by running a conversion
## function on the value the `Err` holds. Then return that new result.
##
## (If the result is `Ok`, this has no effect. Use `try` to transform an `Ok`.)
##
## >>> Result.onErr (Ok 10) \errorNum -> Str.toNat errorNum
##
## >>> Result.onErr (Err "42") \errorNum -> Str.toNat errorNum
onErr : Result a err, (err -> Result a otherErr) -> Result a otherErr
onErr = \result, transform ->
    when result is
        Ok v -> Ok v
        Err e -> transform e
