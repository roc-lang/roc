# A monomorphic callable value mutually recursive with a generalized one.
# `is_even` is context-free and compile-time evaluated once; `is_odd` is
# generalized (its output row is implicitly open). Each value's producer is a
# block whose lambda captures a local, and each lambda refers to the other.
is_even : U64 -> Bool
is_even = {
    z = 0
    |n| if n == z { Bool.True } else { match is_odd(n - 1) { Yes => Bool.True, No => Bool.False } }
}
is_odd : U64 -> [Yes, No]
is_odd = {
    z = 0
    |n| if n == z { No } else { if is_even(n - 1) { Yes } else { No } }
}
main! = |args| {
    n = List.len(args) + 3
    parity = if is_even(n) { "even" } else { "odd" }
    answer = match is_odd(n) {
        Yes => "odd"
        No => "even"
    }
    echo!("${parity} ${answer}\n")
    Ok({})
}
