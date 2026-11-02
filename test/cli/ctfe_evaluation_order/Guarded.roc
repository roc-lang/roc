Guarded := [].{
    choose : Bool -> I64
    choose = |take_failure| if take_failure 12.I64 / 0.I64 else 7.I64

    result : I64
    result = Guarded.choose(Bool.False)
}

expect Guarded.result == 7.I64
