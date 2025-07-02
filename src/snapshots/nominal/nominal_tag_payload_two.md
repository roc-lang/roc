# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=file
~~~
# SOURCE
~~~roc
module [Result, ok, is_ok]

Result(ok, err) := [Ok(a), Err(b)]

ok : a -> Result(a, b)
ok = |a| Result.Ok(a)

is_ok : Result(ok, err) -> Bool
is_ok = |result| match result {
    Result.Ok(_) => True
    Result.Err(_) => False
}
~~~
