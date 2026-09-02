recurse : U64 => U64
recurse = |n|
    1 + recurse(n + 1)

main! = |_| {
    value = recurse(0)

    if value == 0 {
        crash "unreachable after recursive overflow"
    } else {
        Ok({})
    }
}
