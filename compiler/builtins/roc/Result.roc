interface Result
    exposes [ Result, isOk, isErr, map, mapErr, after, withDefault ]
    imports [ ]

Result ok err : [ Ok ok, Err err ]

isOk : Result ok err -> Bool
isOk = \result ->
    when result is
        Ok _ -> True
        Err _ -> False

isErr : Result ok err -> Bool
isErr = \result ->
    when result is
        Ok _ -> False
        Err _ -> True

withDefault : Result ok err, ok -> ok
withDefault = \result, default ->
    when result is
        Ok value -> value
        Err _ -> default

map : Result a err, (a -> b) -> Result b err
map = \result, transform ->
    when result is
        Ok v -> Ok (transform v)
        Err e -> Err e

mapErr : Result ok a, (a -> b) -> Result ok b
mapErr = \result, transform ->
    when result is
        Ok v -> Ok v
        Err e -> Err (transform e)

after : Result a err, (a -> Result b err) -> Result b err
after = \result, transform ->
    when result is
        Ok v -> transform v
        Err e -> Err e
