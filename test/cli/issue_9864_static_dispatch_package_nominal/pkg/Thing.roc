Thing := { value : U64 }.{
    new : U64 -> Thing
    new = |value| { value: value }

    value : Thing -> U64
    value = |thing| thing.value
}
