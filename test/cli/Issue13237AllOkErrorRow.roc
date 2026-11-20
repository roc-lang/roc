# Repro for https://github.com/roc-lang/roc/issues/13237: a function whose
# body builds only `Ok` keeps its error row generalized, and each use decides
# its own instance of it.
f : U64 -> Try(U64, _)
f = |n| if n == 0 Ok(0.U64) else Ok(n)

wide : Try(U64, [B])
wide = f(1.U64)

other_row : Try(U64, [C(Str)])
other_row = f(2.U64)

ok_only : U64 -> U64
ok_only = |n| match f(n) {
    Ok(v) => v
}

forwarded : U64 -> Try(U64, [B])
forwarded = |n| {
    v = f(n)?
    if v == 7 Err(B) else Ok(v)
}

expect wide == Ok(1.U64)
expect other_row == Ok(2.U64)
expect ok_only(3.U64) == 3.U64
expect forwarded(4.U64) == Ok(4.U64)
expect forwarded(7.U64) == Err(B)
