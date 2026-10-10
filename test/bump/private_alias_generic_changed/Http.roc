Http :: [].{
    Pair(a, b) : (b, a)
    Err : [Failure(U64), Timeout]

    echo : (Str, U64) -> (Str, U64)
    echo = |value| value

    public_echo : Pair(Str, U64) -> Pair(Str, U64)
    public_echo = |value| value
}
