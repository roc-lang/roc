interface Result
    exposes [ Result, withDefault, map, andThen ]
    imports []

Result e a : [ Ok a, Err e ]

map : Result e a, (a -> b) -> Result e b
map = \result, f ->
    when result is
        Ok v -> Ok (f v)
        Err e -> Err e

withDefault : Result x a, a -> a
withDefault = \result, default ->
    when result is
        Ok v -> v
        Err _ -> default

andThen : Result e a, (a -> Result e b) -> Result e b
andThen = \result, f ->
    when result is
        Ok v -> f v 
        Err e -> Err e
