Adder := {}.{
    add_to : U64 -> (U64 -> U64)
    add_to = |n| |x| x + n

    bumper : U64 -> U64
    bumper = add_to(1)

    table : { f : U64 -> U64 }
    table = { f: add_to(10) }
}
