Thing := { value : U64 }.{
    new : U64 -> Thing
    new = |initial| { value: initial }

    value : Thing -> U64
    value = |thing| thing.value
}
