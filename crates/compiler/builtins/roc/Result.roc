interface Result
    exposes [Result, isOk, isErr, map, mapErr, try, onErr, withDefault]
    imports [Bool.{ Bool }]

## The result of an operation that could fail: either the operation went
## okay, or else there was an error of some sort.
Result ok err : [Ok ok, Err err]

## Returns `Bool.true` if the result indicates a success, else returns `Bool.false`
## ```
## Result.isOk (Ok 5)
## ```
isOk : Result ok err -> Bool
isOk = \result ->
    when result is
        Ok _ -> Bool.true
        Err _ -> Bool.false

## Returns `Bool.true` if the result indicates a failure, else returns `Bool.false`
## ```
## Result.isErr (Err "uh oh")
## ```
isErr : Result ok err -> Bool
isErr = \result ->
    when result is
        Ok _ -> Bool.false
        Err _ -> Bool.true

## If the result is `Ok`, returns the value it holds. Otherwise, returns
## the given default value.
## ```
## Result.withDefault (Ok 7) 42
## Result.withDefault (Err "uh oh") 42
## ```
withDefault : Result ok err, ok -> ok
withDefault = \result, default ->
    when result is
        Ok value -> value
        Err _ -> default

## If the result is `Ok`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Ok` holding the transformed value. If the
## result is `Err`, this has no effect. Use [mapErr] to transform an `Err`.
## ```
## Result.map (Ok 12) Num.neg
## Result.map (Err "yipes!") Num.neg
## ```
##
## Functions like `map` are common in Roc; see for example [List.map],
## `Set.map`, and `Dict.map`.
map : Result a err, (a -> b) -> Result b err
map = \result, transform ->
    when result is
        Ok v -> Ok (transform v)
        Err e -> Err e

## If the result is `Err`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Err` holding the transformed value. If
## the result is `Ok`, this has no effect. Use [map] to transform an `Ok`.
## ```
## Result.mapErr (Err "yipes!") Str.isEmpty
## Result.mapErr (Ok 12) Str.isEmpty
## ```
mapErr : Result ok a, (a -> b) -> Result ok b
mapErr = \result, transform ->
    when result is
        Ok v -> Ok v
        Err e -> Err (transform e)

## If the result is `Ok`, transforms the entire result by running a conversion
## function on the value the `Ok` holds. Then returns that new result. If the
## result is `Err`, this has no effect. Use `onErr` to transform an `Err`.
## ```
## Result.try (Ok -1) \num -> if num < 0 then Err "negative!" else Ok -num
## Result.try (Err "yipes!") \num -> if num < 0 then Err "negative!" else Ok -num
## ```
try : Result a err, (a -> Result b err) -> Result b err
try = \result, transform ->
    when result is
        Ok v -> transform v
        Err e -> Err e

## If the result is `Err`, transforms the entire result by running a conversion
## function on the value the `Err` holds. Then returns that new result. If the
## result is `Ok`, this has no effect. Use `try` to transform an `Ok`.
## ```
## Result.onErr (Ok 10) \errorNum -> Str.toNat errorNum
## Result.onErr (Err "42") \errorNum -> Str.toNat errorNum
## ```
onErr : Result a err, (err -> Result a otherErr) -> Result a otherErr
onErr = \result, transform ->
    when result is
        Ok v -> Ok v
        Err e -> transform e

## Changes value of Ok. Can be used instead of `when f is`.
## ```
## Result.onErr (Ok 123) "something"
## Result.onErr (Err 456) "something"
## ```
setOk : Result ok err, dif -> Result dif err
setOk = \result, ok ->
    when result is
        Ok _ -> Ok ok
        Err e -> Err e

expect setOk (Ok 123) "something" == Ok "something"
expect setOk (Err 456) "something" == Err 456

## Changes value of Err. Can be used instead of `when f is`.
## ```
## Result.onErr (Ok 123) "something"
## Result.onErr (Err 456) "something"
## ```
setErr : Result ok err, dif -> Result ok dif
setErr = \result, err ->
    when result is
        Ok v -> Ok v
        Err _ -> Err err

expect setErr (Ok 123) "something" == Ok 123
expect setErr (Err 456) "something" == Err "something"
