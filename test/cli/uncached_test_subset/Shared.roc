Shared := [].{
    check : I64 -> I64
    check = |value| {
        expect value != 2
        value + 10
    }
}
