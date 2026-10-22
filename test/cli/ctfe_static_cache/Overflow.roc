import Static

Overflow := [].{
    value : I64
    value = Static.read(18446744073709551615)
}

expect Overflow.value == 0.I64
