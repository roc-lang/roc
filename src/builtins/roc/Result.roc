module [
    Result,
    is_ok,
    is_err,
    is_eq,
    map_ok,
    map_err,
    map_both,
    map2,
    try,
    on_err,
    on_err!,
    with_default,
]

import Bool exposing [Bool.*]

## The result of an operation that could fail: either the operation went
## okay, or else there was an error of some sort.
Result(ok, err) := [Ok(ok), Err(err)]

## Returns `Bool.true` if the result indicates a success, else returns `Bool.false`
## ```roc
## Ok(5).is_ok()
## ```
is_ok : Result(ok, err) -> Bool
is_ok = |result| match result {
    Ok(_) => Bool.true
    Err(_) => Bool.false
}

## Returns `Bool.true` if the result indicates a failure, else returns `Bool.false`
## ```roc
## Err("uh oh").is_err()
## ```
is_err : Result(ok, err) -> Bool
is_err = |result| match result {
    Ok(_) => Bool.false
    Err(_) => Bool.true
}

## If the result is `Ok`, returns the value it holds. Otherwise, returns
## the given default value.
## ```roc
## Ok(7).with_default(42)
## Err("uh oh").with_default(42)
## ```
with_default : Result(ok, err), ok -> ok
with_default = |result, default| match result {
        Ok(value) => value
        Err(_) => default
}

## If the result is `Ok`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Ok` holding the transformed value. If the
## result is `Err`, this has no effect. Use [map_err] to transform an `Err`.
## ```roc
## Ok(12).map_ok(Num.neg)
## Err("yipes!").map_ok(Num.neg)
## ```
##
## Functions like `map` are common in Roc; see for example [List.map],
## `Set.map`, and `Dict.map`.
map_ok : Result(a, err), (a -> b) -> Result(b, err)
map_ok = |result, transform| match result {
    Ok(v) => Ok(transform(v))
    Err(e) => Err(e)
}

## If the result is `Err`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Err` holding the transformed value. If
## the result is `Ok`, this has no effect. Use [map] to transform an `Ok`.
## ```roc
## Err("yipes!").map_err(Str.is_empty)
## Ok(12).map_err(Str.is_empty)
## ```
map_err : Result(ok, a), (a -> b) -> Result(ok, b)
map_err = |result, transform| match result {
    Ok(v) => Ok(v)
    Err(e) => Err(transform(e))
}

## Maps both the `Ok` and `Err` values of a `Result` to new values.
map_both : Result(ok1, err1), (ok1 -> ok2), (err1 -> err2) -> Result(ok2, err2)
map_both = |result, ok_transform, err_transform| match result {
    Ok(val) => Ok(ok_transform(val))
    Err(err) => Err(err_transform(err))
}

## Maps the `Ok` values of two `Result`s to a new value using a given transformation,
## or returns the first `Err` value encountered.
map2 : Result(a, err), Result(b, err), (a, b -> c) -> Result(c, err)
map2 = |first_result, second_result, transform| match (first_result, second_result) {
    (Ok(first), Ok(second)) => Ok(transform(first, second))
    (Err(err), _) => Err(err)
    (_, Err(err)) => Err(err)
}

## If the result is `Ok`, transforms the entire result by running a conversion
## function on the value the `Ok` holds. Then returns that new result. If the
## result is `Err`, this has no effect. Use `on_err` to transform an `Err`.
## ```roc
## Ok(-1).try(|num| if num < 0 then Err("negative!") else Ok(-num))
## Err("yipes!").try(|num| if num < 0 then Err("negative!") else Ok(-num))
## ```
try : Result(a, err), (a -> Result(b, err)) -> Result(b, err)
try = |result, transform| match result {
    Ok(v) => transform(v)
    Err(e) => Err(e)
}

## If the result is `Err`, transforms the entire result by running a conversion
## function on the value the `Err` holds. Then returns that new result. If the
## result is `Ok`, this has no effect. Use `try` to transform an `Ok`.
## ```roc
## Result.on_err(Ok(10), (\error_num -> Str.to_u64(error_num)))
## Result.on_err(Err("42"), (\error_num -> Str.to_u64(error_num)))
## ```
on_err : Result(a, err), (err -> Result(a, other_err)) -> Result(a, other_err)
on_err = |result, transform| match result {
    Ok(v) => Ok(v)
    Err(e) => transform(e)
}

## Like [on_err], but it allows the transformation function to produce effects.
##
## ```roc
## Err("missing user").on_err(|msg| {
##     Stdout.line!("ERROR: ${msg}")?
##     Err(msg)
## })
## ```
on_err! : Result(a, err), (err => Result(a, other_err)) => Result(a, other_err)
on_err! = |result, transform!| match result {
    Ok(v) => Ok(v)
    Err(e) => transform!(e)
}

## Implementation of [Bool.Eq].  Checks if two results that have both `ok` and `err` types that are `Eq` are themselves equal.
##
## ```roc
## Ok("Hello").is_eq(Ok("Hello"))
## ```
is_eq : Result(ok, err), Result(ok, err) -> Bool where ok.Eq, err.Eq
is_eq = |r1, r2| match (r1, r2) {
    (Ok(ok1), Ok(ok2)) => ok1 == ok2
    (Err(err1), Err(err2)) => err1 == err2
}
