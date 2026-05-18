B :: [].{
    Thing := [Thing(I64)].{
        is_eq : Thing, Thing -> Bool
        is_eq = |_, _| Bool.True
    }

    a : Thing
    a = Thing(1)

    b : Thing
    b = Thing(2)
}
