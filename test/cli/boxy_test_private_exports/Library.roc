Library := [].{
    make : I64 -> (I64 -> I64)
    make = |offset| |n| n + offset

    call : (I64 -> I64), I64 -> I64
    call = |f, n| f(n)

    run : I64 -> I64
    run = |n| {
        expect n >= 0
        call(make(1), n)
    }
}

expect {
    nested = |offset| |n| n + offset
    f = nested(10)
    Library.call(f, 2) == 12
}
