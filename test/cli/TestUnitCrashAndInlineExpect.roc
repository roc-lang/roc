TestUnitCrashAndInlineExpect := {}

# Run from test units' images under `roc test --specialize=no --opt=dev`
# with a cache: a crash in one root leaves the next root unaffected, and an
# inline expect in a called function reports at its own line.

halve : U64 -> U64
halve = |n| {
    expect n % 2 == 0
    n // 2
}

first : List(U64) -> U64
first = |items|
    match List.first(items) {
        Ok(item) => item
        Err(_) => crash "the list was empty"
    }

expect first([]) == 0

expect halve(4) == 2

expect halve(3) == 1
