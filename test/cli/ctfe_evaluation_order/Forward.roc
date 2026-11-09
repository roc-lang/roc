Forward := [].{
    result : U64
    result = Forward.apply(|_| Forward.base + 5.U64)

    apply : ({} -> U64) -> U64
    apply = |callback| callback({})

    base : U64
    base = 30.U64
}

expect Forward.result == 35.U64
