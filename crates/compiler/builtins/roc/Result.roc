module [
    Result,
    is_ok,
    is_err,
    map_ok,
    map_err,
    map_both,
    map2,
    try,
    on_err,
    on_err!,
    with_default,
]

import Bool exposing [Bool]

## The result of an operation that could fail: either the operation went
## okay, or else there was an error of some sort.
Result ok err : [Ok ok, Err err]

## Returns `Bool.true` if the result indicates a success, else returns `Bool.false`
## ```roc
## Result.is_ok(Ok(5))
## ```
is_ok : Result ok err -> Bool
is_ok = |result|
    when result is
        Ok(_) -> Bool.true
        Err(_) -> Bool.false

## Returns `Bool.true` if the result indicates a failure, else returns `Bool.false`
## ```roc
## Result.is_err(Err("uh oh"))
## ```
is_err : Result ok err -> Bool
is_err = |result|
    when result is
        Ok(_) -> Bool.false
        Err(_) -> Bool.true

## If the result is `Ok`, returns the value it holds. Otherwise, returns
## the given default value.
## ```roc
## Result.with_default(Ok(7), 42)
## Result.with_default(Err("uh oh"), 42)
## ```
with_default : Result ok err, ok -> ok
with_default = |result, default|
    when result is
        Ok(value) -> value
        Err(_) -> default

## If the result is `Ok`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Ok` holding the transformed value. If the
## result is `Err`, this has no effect. Use [map_err] to transform an `Err`.
## ```roc
## Result.map_ok(Ok(12), Num.neg)
## Result.map_ok(Err("yipes!"), Num.neg)
## ```
##
## Functions like `map` are common in Roc; see for example [List.map],
## `Set.map`, and `Dict.map`.
map_ok : Result a err, (a -> b) -> Result b err
map_ok = |result, transform|
    when result is
        Ok(v) -> Ok(transform(v))
        Err(e) -> Err(e)

## If the result is `Err`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Err` holding the transformed value. If
## the result is `Ok`, this has no effect. Use [map] to transform an `Ok`.
## ```roc
## Result.map_err(Err("yipes!"), Str.is_empty)
## Result.map_err(Ok(12), Str.is_empty)
## ```
map_err : Result ok a, (a -> b) -> Result ok b
map_err = |result, transform|
    when result is
        Ok(v) -> Ok(v)
        Err(e) -> Err(transform(e))

## Maps both the `Ok` and `Err` values of a `Result` to new values.
map_both : Result ok1 err1, (ok1 -> ok2), (err1 -> err2) -> Result ok2 err2
map_both = |result, ok_transform, err_transform|
    when result is
        Ok(val) -> Ok(ok_transform(val))
        Err(err) -> Err(err_transform(err))

## Maps the `Ok` values of two `Result`s to a new value using a given transformation,
## or returns the first `Err` value encountered.
map2 : Result a err, Result b err, (a, b -> c) -> Result c err
map2 = |first_result, second_result, transform|
    when (first_result, second_result) is
        (Ok(first), Ok(second)) -> Ok(transform(first, second))
        (Err(err), _) -> Err(err)
        (_, Err(err)) -> Err(err)

## If the result is `Ok`, transforms the entire result by running a conversion
## function on the value the `Ok` holds. Then returns that new result. If the
## result is `Err`, this has no effect. Use `on_err` to transform an `Err`.
## ```roc
## Result.try(Ok(-1), (\num -> if num < 0 then Err("negative!") else Ok(-num)))
## Result.try(Err("yipes!"), (\num -> if num < 0 then Err("negative!") else Ok(-num)))
## ```
try : Result a err, (a -> Result b err) -> Result b err
try = |result, transform|
    when result is
        Ok(v) -> transform(v)
        Err(e) -> Err(e)

## If the result is `Err`, transforms the entire result by running a conversion
## function on the value the `Err` holds. Then returns that new result. If the
## result is `Ok`, this has no effect. Use `try` to transform an `Ok`.
## ```roc
## Result.on_err(Ok(10), (\error_num -> Str.to_u64(error_num)))
## Result.on_err(Err("42"), (\error_num -> Str.to_u64(error_num)))
## ```
on_err : Result a err, (err -> Result a other_err) -> Result a other_err
on_err = |result, transform|
    when result is
        Ok(v) -> Ok(v)
        Err(e) -> transform(e)

## Like [on_err], but it allows the transformation function to produce effects.
##
## ```roc
## Result.on_err(
##     Err("missing user"),
##     \msg ->
##         Stdout.line!("ERROR: ${msg}")?
##         Err(msg),
## )
## ```
on_err! : Result a err, (err => Result a other_err) => Result a other_err
on_err! = |result, transform!|
    when result is
        Ok(v) -> Ok(v)
        Err(e) -> transform!(e)
