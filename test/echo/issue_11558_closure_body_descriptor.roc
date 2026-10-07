# repro for https://github.com/roc-lang/roc/issues/11558
#
# The returned closure's signature never mentions `a`, but its body builds a
# `List(a)`, so it needs `make_counter`'s descriptor for `a`. Every backend,
# including `--specialize=no`, prints the same result.
make_counter : a -> (U64 -> Str)
make_counter = |_x| |n| {
    xs : List(a)
    xs = List.with_capacity(n)
    Str.inspect(xs)
}

main! = |_args| {
    count = make_counter("hi")
    echo!("${count(3)}\n")
    Ok({})
}
