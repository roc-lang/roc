MyType(val) := [A(val), B].{
    from_interpolation : Str, Iter((val, Str)) -> MyType(val)
    from_interpolation = |_, _| B
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
