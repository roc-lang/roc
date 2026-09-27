Http :: [].{
    Err : [Failure(Str), Timeout]

    echo : (U64, Str) -> (U64, Str)
    echo = |value| value

    public_echo : (U64, Str) -> (U64, Str)
    public_echo = |value| value
}
