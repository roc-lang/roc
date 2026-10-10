Http :: [].{
    Pair(a, b) : { first: b, second: a }
    Err : [Failure(Str), Timeout]

    echo : (U64, Str) -> (U64, Str)
    echo = |value| value

    public_echo : Pair(Str, U64) -> Pair(Str, U64)
    public_echo = |value| value
}
