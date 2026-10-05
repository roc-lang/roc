ErasedTailCalls := {}

# Without specialization every capturing lambda is an erased function value,
# so each of these cycles goes through an erased call in tail position.

through_capturing_lambda : U64 -> U64
through_capturing_lambda = |n| if n == 0 0 else 0 |> (|_| through_capturing_lambda(n - 1))

expect through_capturing_lambda(30_000) == 0

through_named_lambda : U64, Str -> U64
through_named_lambda = |n, label| if n == 0 Str.count_utf8_bytes(label) else {
    next = |kept| through_named_lambda(n - 1, Str.concat(kept, ""))
    next(label)
}

expect through_named_lambda(30_000, "a fairly long label that lives on the heap") == 42

# The function value is chosen at runtime and called in tail position.
bounce : U64, (U64 -> U64), (U64 -> U64) -> U64
bounce = |n, even, odd| if n == 0 even(0) else bounce(n - 1, odd, even)

chain : U64, (U64 -> U64) -> U64
chain = |n, done| if n == 0 done(7) else chain(n - 1, |x| done(x))

expect bounce(30_000, |x| x + 1, |x| x + 2) == 1
expect chain(1_000, |x| x) == 7
