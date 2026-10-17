InlinedCapturingLambdas := {}

# Each lambda below is called once, where it is written, and captures values
# from its enclosing function.

# The capture is consumed on one branch and kept by the caller on the other.
grow : List(U64), Bool -> List(U64)
grow = |xs, flag| {
    extended = {} |> (|_| if flag List.append(xs, 9) else xs)
    if flag extended else List.concat(extended, xs)
}

expect grow([1, 2], Bool.True) == [1, 2, 9]
expect grow([1, 2], Bool.False) == [1, 2, 1, 2]

# A function value built inside the lambda captures the lambda's own captures.
scaled : List(U64), U64, U64 -> List(U64)
scaled = |xs, factor, offset| 0 |> (|base| List.map(xs, |x| x * factor + offset + base))

expect scaled([1, 2, 3], 10, 1) == [11, 21, 31]

# A heap string captured and read after the lambda returns.
describe : Str, U64 -> Str
describe = |label, n| {
    counted = n |> (|k| Str.concat(label, Str.repeat("!", k)))
    Str.concat(counted, label)
}

expect describe("a fairly long label that lives on the heap", 2) == "a fairly long label that lives on the heap!!a fairly long label that lives on the heap"

# Lambdas nested in lambdas, each capturing from every enclosing scope.
nested : U64, U64 -> U64
nested = |a, b| 1 |> (|c| 2 |> (|d| a * 1000 + b * 100 + c * 10 + d))

expect nested(4, 3) == 4312

# Recursion through a capturing lambda.
capturing_lambda : U64 -> U64
capturing_lambda = |n| if n == 0 0 else 0 |> (|_| capturing_lambda(n - 1))

expect capturing_lambda(100_000) == 0
