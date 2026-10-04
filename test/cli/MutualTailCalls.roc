MutualTailCalls := {}

# Every recursive call below is a tail call to a different function, so each
# chain runs in constant stack space however long it is.

is_even : U64 -> Bool
is_even = |n| if n == 0 True else is_odd(n - 1)

is_odd : U64 -> Bool
is_odd = |n| if n == 0 False else is_even(n - 1)

expect is_even(200_000) == True
expect is_odd(200_001) == True

# A lambda called where it is written, with and without a captured variable.
through_lambda = |n| if n == 0 0 else n |> (|m| through_lambda(m - 1))

through_capturing_lambda = |n| if n == 0 0 else 0 |> (|_| through_capturing_lambda(n - 1))

expect through_lambda(200_000) == 0
expect through_capturing_lambda(200_000) == 0

# Refcounted arguments are handed from one frame to the next.
ping : U64, List(U64), Str -> U64
ping = |n, acc, label| if n == 0 List.len(acc) + Str.count_utf8_bytes(label) else pong(n - 1, List.append(acc, n), label)

pong : U64, List(U64), Str -> U64
pong = |n, acc, label| if n == 0 List.len(acc) + Str.count_utf8_bytes(label) else ping(n - 1, acc, Str.concat(label, ""))

expect ping(100_000, [], "a fairly long label that lives on the heap") == 50_000 + 42

# More argument words than there are argument registers, and a different
# amount in each function, so each call rebuilds its callee's stack arguments.
wide_a : U64, Str, Str, Str, Str -> U64
wide_a = |n, a, b, c, d| if n == 0 Str.count_utf8_bytes(a) + Str.count_utf8_bytes(b) + Str.count_utf8_bytes(c) + Str.count_utf8_bytes(d) else wide_b(n - 1, d, a)

wide_b : U64, Str, Str -> U64
wide_b = |n, a, b| if n == 0 Str.count_utf8_bytes(a) + Str.count_utf8_bytes(b) else wide_c(n - 1, 1, 2, 3, 4, 5, 6, 7, 8, a, b)

wide_c : U64, U64, U64, U64, U64, U64, U64, U64, U64, Str, Str -> U64
wide_c = |n, p, q, r, s, t, u, v, w, a, b| if n == 0 p + q + r + s + t + u + v + w + Str.count_utf8_bytes(a) + Str.count_utf8_bytes(b) else wide_a(n - 1, a, b, a, b)

expect wide_a(150_000, "one", "three", "fives", "sevens!") == 3 + 7 + 3 + 7
expect wide_a(150_001, "one", "three", "fives", "sevens!") == 7 + 3
expect wide_a(150_002, "one", "three", "fives", "sevens!") == 36 + 7 + 3

# A result too large for registers is written straight to the first caller.
Big : { a : U64, b : U64, c : U64, d : Str }

big_a : U64, Big -> Big
big_a = |n, acc| if n == 0 acc else big_b(n - 1, { ..acc, a: acc.a + 1 })

big_b : U64, Big -> Big
big_b = |n, acc| if n == 0 acc else big_a(n - 1, { ..acc, b: acc.b + 2 })

expect big_a(100_000, { a: 0, b: 0, c: 7, d: "kept" }) == { a: 50_000, b: 100_000, c: 7, d: "kept" }
