Factory := [].{
    make = |captured| |_| captured
    offset : I64 -> (I64 -> I64)
    offset = |amount| |value| value + amount
}
