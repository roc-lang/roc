Factory := [].{
    direct : a -> Str
    direct = |value| Factory.make(value)({})

    closed : {} -> ({} -> Str)
    closed = |_| Factory.make(7.U64)

    make : a -> ({} -> Str)
    make = |value| {
        Local := [Local(a)].{
            is_eq : Local, Local -> Bool
            is_eq = |_, _| Bool.False
        }
        capture(Local.Local(value))
    }
}

# Calling the captured equality dictionary must retain Local's False override.
capture : a -> ({} -> Str) where [a.is_eq : a, a -> Bool]
capture = |value| |_| {
    if value == value {
        "wrong override"
    } else {
        Str.inspect(value)
    }
}
