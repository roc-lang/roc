MyType(val) := [A(val), B].{
    from_interpolation : List(Str) -> Try((List(val) -> MyType(val)), [InvalidInterpolation(Str)])
    from_interpolation = |_| Ok(|_| B)
}

g = |x, y| {
    res = "hello ${x}"
    (res, y)
}

main! = |_| {
    val : (MyType(Str), MyType(Str))
    val = g({}, B)
    {}
}
