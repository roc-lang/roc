module [
    Result,
    is_ok,
    is_err,
    is_eq,
    map_ok,
    map_err,
    on_err,
    on_err!,
    map_both,
    map2,
    try,
    with_default,
]

import Bool exposing [Bool.*]

## The result of an operation that could fail: either the operation went
## okay, or else there was an error of some sort.
Result(ok, err) := [Ok(ok), Err(err)]

## Returns `Bool.true` if the result indicates a success, else returns `Bool.false`.
## ```roc
## Ok(5).is_ok()
## ```
is_ok : Result(ok, err) -> Bool
is_ok = |result| match result {
    Result.Ok(_) => Bool.true
    Result.Err(_) => Bool.false
}

## Returns `Bool.true` if the result indicates a failure, else returns `Bool.false`.
## ```roc
## Err("uh oh").is_err()
## ```
is_err : Result(ok, err) -> Bool
is_err = |result| match result {
    Result.Ok(_) => Bool.false
    Result.Err(_) => Bool.true
}

## If the result is `Ok`, returns the value it holds. Otherwise, returns
## the given default value.
##
## Note: This function should be used sparingly, because it hides that an error
## happened, which will make debugging harder. Prefer using `?` to forward errors or
## handle them explicitly with `when`.
## ```roc
## Err("uh oh").with_default(42) # = 42
##
## Ok(7).with_default(42) # = 7
## ```
with_default : Result(ok, err), ok -> ok
with_default = |result, default| match result {
        Result.Ok(value) => value
        Result.Err(_) => default
}

## If the result is `Ok`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Ok` holding the transformed value. If the
## result is `Err`, this has no effect. Use [map_err] to transform an `Err`.
## ```roc
## Ok(12).map_ok(Num.neg) # = Ok(-12)
##
## Err("yipes!").map_ok(Num.neg) # = Err("yipes!")
## ```
##
## Functions like `map` are common in Roc; see for example [List.map],
## `Set.map`, and `Dict.map`.
map_ok : Result(a, err), (a -> b) -> Result(b, err)
map_ok = |result, transform| match result {
    Result.Ok(v) => Result.Ok(transform(v))
    Result.Err(e) => Result.Err(e)
}

## If the result is `Err`, transforms the value it holds by running a conversion
## function on it. Then returns a new `Err` holding the transformed value. If
## the result is `Ok`, this has no effect. Use [map] to transform an `Ok`.
## ```roc
## [].last().map_err(|_| ProvidedListIsEmpty) # = Err(ProvidedListIsEmpty)
##
## [4].last().map_err(|_| ProvidedListIsEmpty) # = Ok(4)
## ```
map_err : Result(ok, a), (a -> b) -> Result(ok, b)
map_err = |result, transform| match result {
    Result.Ok(v) => Result.Ok(v)
    Result.Err(e) => Result.Err(transform(e))
}

## If the result is `Err`, transforms the entire result by running a conversion
## function on the value the `Err` holds. Then returns that new result. If the
## result is `Ok`, this has no effect. Use `?` or [try] to transform an `Ok`.
## ```roc
## Result.on_err(Ok(10), Str.to_u64) # = Ok(10)
##
## Result.on_err(Err("42"), Str.to_u64) # = Ok(42)
##
## Result.on_err(Err("string"), Str.to_u64) # = Err(InvalidNumStr)
## ```
on_err : Result(a, err), (err -> Result(a, other_err)) -> Result(a, other_err)
on_err = |result, transform| match result {
    Result.Ok(v) => Result.Ok(v)
    Result.Err(e) => transform(e)
}

expect Result.on_err(Ok(10), Str.to_u64) == Result.Ok(10)
expect Result.on_err(Err("42"), Str.to_u64) == Result.Ok(42)
expect Result.on_err(Err("string"), Str.to_u64) == Result.Err(InvalidNumStr)

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
    Result.Ok(v) => Result.Ok(v)
    Result.Err(e) => transform!(e)
}

## Maps both the `Ok` and `Err` values of a `Result` to new values.
map_both : Result(ok1, err1), (ok1 -> ok2), (err1 -> err2) -> Result(ok2, err2)
map_both = |result, ok_transform, err_transform| match result {
    Result. Ok(val) => Result.Ok(ok_transform(val))
    Result. Err(err) => Result.Err(err_transform(err))
}

## Maps the `Ok` values of two `Result`s to a new value using a given transformation,
## or returns the first `Err` value encountered.
map2 : Result(a, err), Result(b, err), (a, b -> c) -> Result(c, err)
map2 = |first_result, second_result, transform| match (first_result, second_result) {
    (Result.Ok(first), Result.Ok(second)) => Ok(transform(first, second))
    (Result.Err(err), _) => Result.Err(err)
    (_, Result.Err(err)) => Result.Err(err)
}

## If the result is `Ok`, transforms the entire result by running a conversion
## function on the value the `Ok` holds. Then returns that new result. If the
## result is `Err`, this has no effect. Use `on_err` to transform an `Err`.
##
## We recommend using `?` instead of `try`, it makes the code easier to read.
## ```roc
## Ok(-1).try(|num| if num < 0 then Err("negative!") else Ok(-num)) # = Err("negative!")
##
## Ok(1).try(|num| if num < 0 then Err("negative!") else Ok(-num)) # = Ok(-1)
##
## Err("yipes!").try(|num| if num < 0 then Err("negative!") else Ok(-num)) # = Err("yipes!")
## ```
try : Result(a, err), (a -> Result(b, err)) -> Result(b, err)
try = |result, transform| match result {
    Result.Ok(v) => transform(v)
    Result.Err(e) => Result.Err(e)
}

expect Ok(-1).try(|num| if num < 0 then Err("negative!") else Ok(-num)) == Result.Err("negative!")
expect Ok(1).try(|num| if num < 0 then Err("negative!") else Ok(-num)) == Result.Ok(-1)
expect Err("yipes!").try(|num| if num < 0 then Err("negative!") else Ok(-num)) == Result.Err("yipes!")

## Implementation of [Bool.Eq].  Checks if two results that have both `ok` and `err` types that are `Eq` are themselves equal.
##
## ```roc
## Ok("Hello").is_eq(Ok("Hello"))
## ```
is_eq : Result(ok, err), Result(ok, err) -> Bool where [ok.Eq, err.Eq]
is_eq = |r1, r2| match (r1, r2) {
    (Result.Ok(ok1), Result.Ok(ok2)) => ok1 == ok2
    (Result.Err(err1), Result.Err(err2)) => err1 == err2
}

expect Result.Ok(1) == Result.Ok(1)
expect Result.Ok(2) != Result.Ok(1)
expect Result.Err("Foo") == Result.Err("Foo")
expect Result.Err("Bar") != Result.Err("Foo")
expect Result.Ok("Foo") != Result.Err("Foo")
