import InternalHttp

Local(a, b) : InternalHttp.Pair(b, a)

Http :: [].{
    Pair(a, b) : Local(a, b)
    Err : InternalHttp.Err(Str)

    echo : Local(Str, U64) -> Local(Str, U64)
    echo = |value| value

    public_echo : Pair(Str, U64) -> Pair(Str, U64)
    public_echo = |value| value
}
