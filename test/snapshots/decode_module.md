# META
~~~ini
description=Test Decode module basic functionality
type=snippet
~~~
# SOURCE
~~~roc
# Test that Decode module types are recognized
DecodeResult(a) := [Ok(a), Err(Str)]

decodeU8 : List(U8) -> DecodeResult(U8)
decodeU8 = |bytes|
    match bytes {
        [b, ..] => DecodeResult.Ok(b)
        [] => DecodeResult.Err("empty bytes")
    }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
